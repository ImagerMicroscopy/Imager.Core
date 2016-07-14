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
import Data.Text (Text)
import qualified Data.Vector.Storable as V
import System.Clock

import OOSeaBreeze
import LightSources

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
                         , dpIrradiation :: !IrradiationParams
                       }

data IrradiationParams = IrradiationParams {
                             ipLightSourceName :: !Text
                           , ipLightSourceChannel :: !Text
                           , ipPower :: !Double
                         } 
                         deriving (Show)

data ProgramEnvironment = ProgramEnvironment {
                              peSpectrometer :: (DeviceID, FeatureID)
                            , peLightSources :: [LightSource]
                            , peSpectraMVar :: MVar ([[(V.Vector Double, Double)]])
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

executeIrradiationProgram :: IrradiationProgram -> ProgramEnvironment -> IO ()
executeIrradiationProgram (IrradiationProgram steps detection) env =
    (getTime Monotonic >>= \startTime ->
    putStrLn "will execute acquisition" >> putStrLn (show (initialAcquisitionStep : steps)) >>
    mapM_ (executeStep env startTime detection) (initialAcquisitionStep : steps))
        `finally` (deactivateAllLightSources lightSources)
    where
        initialAcquisitionStep :: ProgramStep   -- makes sure a spectrum gets acquired before the acquisition
        initialAcquisitionStep = ProgramStep 0.0 1 []

        executeStep :: ProgramEnvironment -> TimeSpec -> [DetectionParams] -> ProgramStep -> IO ()
        executeStep env startTime detParams ps =
            forM_ (replicate (psNTimesToPerform ps) ps) $ \step ->
                executeSingleIrradiationInStep detParams step >>= \newSpectra ->
                modifyMVar_ (peSpectraMVar env) (\previousSpectra ->
                    when (length previousSpectra > 100) (error "too many async spectra stored") >>
                    return (previousSpectra ++ [map toSecondsFromStart newSpectra]))
            where
                toSecondsFromStart (vec, t) = (vec, (*) 1.0e-9 . fromIntegral . timeSpecAsNanoSecs $ diffTimeSpec t startTime)
        
        executeSingleIrradiationInStep :: [DetectionParams] -> ProgramStep -> IO [(V.Vector Double, TimeSpec)]
        executeSingleIrradiationInStep detParams ProgramStep{..} =
            runExceptT (
                ExceptT (enableLightSources lightSources psIrradiation) >>
                ExceptT (Right <$> threadDelay (floor $ psIrradiationDuration * 1.0e6)) >>
                ExceptT (disableLightSources lightSources psIrradiation)) >>= \irrResult ->
            case irrResult of
                Left err -> error err
                Right _ -> mapM executeStepDetection detParams

        executeStepDetection :: DetectionParams -> IO (V.Vector Double, TimeSpec)
        executeStepDetection params =
            getTime Monotonic >>= \timeStamp ->
            executeDetection (spectrometerDeviceID, spectrometerFeatureID) lightSources  params >>= \detResult ->
            case detResult of
                Right v -> return (v, timeStamp)
                Left err -> error err

        lightSources = peLightSources env
        spectrometerDeviceID = fst (peSpectrometer env)
        spectrometerFeatureID = snd (peSpectrometer env)
        
executeDetection :: (DeviceID, FeatureID) -> [LightSource] -> DetectionParams -> IO (Either String (V.Vector Double))
executeDetection (dID, fID) lss DetectionParams{..} =
    runExceptT (
        ExceptT (enableLightSources lss [dpIrradiation]) >>
        ExceptT (setIntegrationTimeMicros dID fID (floor $ dpExposureTime * 1.0e6)) >>
        ExceptT (measureAveragedSpectrum dID fID dpNSpectraToAverage) >>= \spectrum ->
        ExceptT (disableLightSources lss [dpIrradiation]) >>
        ExceptT (return $ Right spectrum))

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
            if ((isJust $ lookupMaybeLightSource lightSources ipLightSourceName) && (within ipPower 0.0 100.0))
                then Right ()
                else Left "invalid irradiation params"
        validateDetectionParams :: DetectionParams -> Either String ()
        validateDetectionParams DetectionParams{..} =
            if ((within dpExposureTime 3.8e-3 10) && (within dpNSpectraToAverage 1 1000) && (isRight $ validateIrradiation dpIrradiation))
                then Right ()
                else Left "invalid detection params"
        validateProgramStep :: ProgramStep -> Either String ()
        validateProgramStep ProgramStep{..} =
            if ((within psIrradiationDuration 0.0 3600) && (within psNTimesToPerform 1 5000) && (all isRight . map validateIrradiation $ psIrradiation))
                then Right ()
                else Left "invalid program step"
        within :: (Ord a) => a -> a -> a -> Bool
        within a b c = (a >= b) && (a <= c)

nAcquisitionsInProgram :: IrradiationProgram -> Int
nAcquisitionsInProgram IrradiationProgram{..} = sum (map nAcquisitionsInStep ipSteps) + 1
    where
        nAcquisitionsInStep step = psNTimesToPerform step
