{-# LANGUAGE BangPatterns, RecordWildCards, OverloadedStrings #-}

module IrradiationProgram where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import System.Clock

import LightSources
import Detector
import MiscUtils

data IrradiationProgram = IrradiationProgram {
                   ipSteps :: [ProgramStep]
                 , ipDetection :: [DetectionParams]
               }

data ProgramStep = ProgramStep {
                       psIrradiationDuration :: !Double
                     , psNTimesToPerform :: !Int
                     , psIrradiation :: [IrradiationParams]
                   }
                   deriving (Show)

data DetectionParams = DetectionParams {
                           dpExposureTime :: !Double
                         , dpNSpectraToAverage :: !Int
                         , dpIrradiation :: [IrradiationParams]
                       }

data IrradiationParams = IrradiationParams {
                             ipLightSourceName :: !Text
                           , ipLightSourceChannel :: ![Text]
                           , ipPower :: ![Double]
                         }
                         deriving (Show)

data ProgramEnvironment a = ProgramEnvironment {
                                peDetector :: a
                              , peLightSources :: [LightSource]
                              , peDataMVar :: MVar [[AcquiredData]]
                            }

instance FromJSON IrradiationProgram where
    parseJSON (Object v) =
        IrradiationProgram <$> v .: "programsteps"
                           <*> v .: "detection"
    parseJSON _ = fail "can't decode irradiation program"
instance ToJSON IrradiationProgram where
    toEncoding (IrradiationProgram steps detection) =
        pairs ("programsteps" .= steps <> "detection" .= detection)
    toJSON _ = error "no toJSON"

instance FromJSON ProgramStep where
    parseJSON (Object v) =
        ProgramStep <$> v .: "irradiationduration"
                    <*> v .: "ntimestoperform"
                    <*> v .: "irradiation"
    parseJSON _ = fail "can't decode program step"
instance ToJSON ProgramStep where
    toEncoding (ProgramStep duration nTimesToPerform irr) =
        pairs ("irradiationduration" .= duration <> "ntimestoperform" .= nTimesToPerform <> "irradiation" .= irr)
    toJSON _ = error "no toJSON"

instance FromJSON DetectionParams where
    parseJSON (Object v) =
        DetectionParams <$> v .: "exposuretime"
                        <*> v .: "nspectra"
                        <*> v .: "irradiation"
    parseJSON _ = fail "can't decode detection params"
instance ToJSON DetectionParams where
    toEncoding (DetectionParams expTime nSpectra irr) =
        pairs ("exposuretime" .= expTime <> "nspectra" .= nSpectra <> "irradiation" .= irr)
    toJSON _ = error "no toJSON"

instance FromJSON IrradiationParams where
    parseJSON (Object v) =
        IrradiationParams <$> v .: "lightsourcename"
                          <*> v .: "lightsourcechannel"
                          <*> v .: "lightsourcepower"
    parseJSON _ = fail "can't decode irradiation params"
instance ToJSON IrradiationParams where
    toEncoding (IrradiationParams lName channel power) =
        pairs ("lightsourcename" .= lName <> "lightsourcechannel" .= channel <> "lightsourcepower" .= power)
    toJSON _ = error "no toJSON"

executeIrradiationProgram :: Detector a => IrradiationProgram -> ProgramEnvironment a -> IO ()
executeIrradiationProgram prog@(IrradiationProgram steps detection) env =
    (getTime Monotonic >>= \startTime ->
    putStrLn "will execute acquisition" >> putStrLn (show (initialAcquisitionStep : steps)) >>
    mapM_ (executeStep env startTime detection) (initialAcquisitionStep : steps))
        `finally` closeUsedLightsources
    where
        closeUsedLightsources = mapM_ (\n -> deactivateLightSource (lookupLightSource lightSources n)) lightSourceNamesInProgram
        lightSourceNamesInProgram = lightsourceNamesUsedInProgram prog
        initialAcquisitionStep :: ProgramStep   -- makes sure a spectrum gets acquired before the acquisition
        initialAcquisitionStep = ProgramStep 0.0 1 []

        executeStep :: Detector a => ProgramEnvironment a -> TimeSpec -> [DetectionParams] -> ProgramStep -> IO ()
        executeStep env startTime detParams ps
            | (irradiationDuration == 0.0) && (length detParams == 1) = -- no irradiation pause and 1 kind of detection - don't open and close shutters needlessly
                  executeFastStep (head detParams) nTimesToPerform startTime dataMVar
            | otherwise =
                  executeSlowStep detParams ps startTime dataMVar
            where
              irradiationDuration = psIrradiationDuration ps
              nTimesToPerform = psNTimesToPerform ps
              dataMVar = peDataMVar env

        executeSlowStep :: [DetectionParams] -> ProgramStep -> TimeSpec -> MVar [[AcquiredData]] -> IO ()
        executeSlowStep detParams ps startTime dataMVar =
          forM_ (replicate (psNTimesToPerform ps) ps) $ \step ->
              executeSingleIrradiationInStep detParams step >>= \newSpectra ->
              addDataToMVar dataMVar startTime newSpectra

        executeSingleIrradiationInStep :: [DetectionParams] -> ProgramStep -> IO [AcquiredData]
        executeSingleIrradiationInStep detParams ProgramStep{..} =
            runExceptT (
                ExceptT (enableLightSources lightSources psIrradiation) >>
                ExceptT (Right <$> threadDelay (floor $ psIrradiationDuration * 1.0e6)) >>
                ExceptT (disableLightSources lightSources psIrradiation)) >>= \irrResult ->
            case irrResult of
                Left err -> error err
                Right _ -> mapM executeStepDetection detParams

        executeStepDetection :: DetectionParams -> IO AcquiredData
        executeStepDetection params =
            executeDetection detector lightSources params >>= \detResult ->
            case detResult of
                Right dat -> return dat
                Left err -> error err

        executeFastStep :: DetectionParams -> Int -> TimeSpec -> MVar [[AcquiredData]] -> IO ()
        executeFastStep detParams nTimesToPerform startTime dataMVar =
            runExceptT (
                ExceptT (enableLightSources lightSources (dpIrradiation detParams)) >>
                ExceptT (Right <$> acquireStreamingData detector (dpExposureTime detParams) 1.0
                            (dpNSpectraToAverage detParams) nTimesToPerform startTime dataMVar) >>
                ExceptT (disableLightSources lightSources (dpIrradiation detParams))) >>= \result ->
            case result of
                Left e -> error e
                Right _ -> return ()
        lightSources = peLightSources env
        detector = peDetector env

executeDetection :: Detector a => a -> [LightSource] -> DetectionParams -> IO (Either String AcquiredData)
executeDetection det lss DetectionParams{..} =
    runExceptT (
        ExceptT (enableLightSources lss dpIrradiation) >>
        ExceptT (acquireData det dpExposureTime 1.0 dpNSpectraToAverage) >>= \acquiredData ->
        ExceptT (disableLightSources lss dpIrradiation) >>
        ExceptT (return $ Right acquiredData))

enableLightSources :: [LightSource] -> [IrradiationParams] -> IO (Either String ())
enableLightSources lss params
    | allLightSourcesKnown = catch (Right <$> mapM_ (\(IrradiationParams sourceName channel power) -> activateLightSource (lookupLightSource lss sourceName) channel power) params)
                                (\e -> return (Left (displayException (e :: IOException))))
    | otherwise = return (Left "unknown light source")
    where
        allLightSourcesKnown = and $ map (isKnownLightSource lss . ipLightSourceName) params
disableLightSources :: [LightSource] -> [IrradiationParams] -> IO (Either String ())
disableLightSources lss params = catch (Right <$> mapM_ (\(IrradiationParams sourceName _ _) -> deactivateLightSource (lookupLightSource lss sourceName)) params)
                                    (\e -> return (Left (displayException (e :: IOException))))
    where
        allLightSourcesKnown = and $ map (isKnownLightSource lss . ipLightSourceName) params

validateIrradiationProgram :: [LightSource] -> IrradiationProgram -> Either String ()
validateIrradiationProgram lightSources IrradiationProgram{..} =
    if (any isLeft validationResults)
        then head . filter isLeft $ validationResults
        else Right ()
    where
        validationResults = map validateProgramStep ipSteps ++ map validateDetectionParams ipDetection
        validateIrradiation :: IrradiationParams -> Either String ()
        validateIrradiation IrradiationParams{..} =
            case (lookupMaybeLightSource lightSources ipLightSourceName) of
              Nothing -> Left "invalid light source name"
              Just ls -> if (validLightSourceChannelsAndPowers ls ipLightSourceChannel ipPower)
                          then Right ()
                          else Left ("invalid light source parameters for " ++ T.unpack ipLightSourceName)
        validateDetectionParams :: DetectionParams -> Either String ()
        validateDetectionParams DetectionParams{..} =
            if ((within dpExposureTime 3.8e-3 10) && (within dpNSpectraToAverage 1 1000) && (all isRight $ map validateIrradiation dpIrradiation))
                then Right ()
                else Left "invalid detection params"
        validateProgramStep :: ProgramStep -> Either String ()
        validateProgramStep ProgramStep{..} =
            if ((within psIrradiationDuration 0.0 3600) && (within psNTimesToPerform 1 5000) && (all isRight $ map validateIrradiation psIrradiation))
                then Right ()
                else Left "invalid program step"

lightsourceNamesUsedInProgram :: IrradiationProgram -> [Text]
lightsourceNamesUsedInProgram (IrradiationProgram steps dets) = S.toList (mconcat (map lightSourceNamesInStep steps) <> mconcat (map lightSourceNamesInDetPars dets))
  where
    lightSourceNamesInStep :: ProgramStep -> Set Text
    lightSourceNamesInStep step = foldr (\irr s -> S.insert (ipLightSourceName irr) s) S.empty (psIrradiation step)
    lightSourceNamesInDetPars :: DetectionParams -> Set Text
    lightSourceNamesInDetPars detPars = foldr (\irr s -> S.insert (ipLightSourceName irr) s) S.empty (dpIrradiation detPars)

nAcquisitionsInProgram :: IrradiationProgram -> Int
nAcquisitionsInProgram IrradiationProgram{..} = sum (map nAcquisitionsInStep ipSteps) + 1
    where
        nAcquisitionsInStep step = psNTimesToPerform step
