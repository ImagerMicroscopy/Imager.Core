{-# LANGUAGE BangPatterns, RecordWildCards, OverloadedStrings #-}

module IrradiationProgram where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
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
                     , psNRepeats :: !Int
                     , psIrradiation :: [IrradiationParams]
                   }

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

data ProgramEnvironment = ProgramEnvironment {
                              peSpectrometer :: (DeviceID, FeatureID)
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
                    <*> v .: "nrepeats"
                    <*> v .: "irradiation"
    parseJSON _ = fail "can't decode program step"
instance ToJSON ProgramStep where
    toEncoding (ProgramStep duration nRepeats irr) =
        pairs ("irradiationduration" .= duration <> "nrepeats" .= nRepeats <> "irradiation" .= irr)
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
    getTime Monotonic >>= \startTime ->
    mapM_ (executeStep env startTime detection) steps
    where
        executeStep :: ProgramEnvironment -> TimeSpec -> [DetectionParams] -> ProgramStep -> IO ()
        executeStep env startTime detParams ps =
            forM_ (replicate (psNRepeats ps) ps) $ \step ->
                executeSingleIrradiationInStep detParams step >>= \newSpectra ->
                modifyMVar_ (peSpectraMVar env) (\previousSpectra ->
                    return (previousSpectra ++ [map toSecondsFromStart newSpectra]))
            where
                toSecondsFromStart (vec, t) = (vec, (*) 1.0e-9 . fromIntegral . timeSpecAsNanoSecs $ diffTimeSpec t startTime)
        
        executeSingleIrradiationInStep :: [DetectionParams] -> ProgramStep -> IO [(V.Vector Double, TimeSpec)]
        executeSingleIrradiationInStep detParams ProgramStep{..} =
            enableLightSources psIrradiation >>
            threadDelay (floor $ psIrradiationDuration * 1.0e6) >>
            mapM executeDetection detParams >>= \spectra ->
            disableLightSources psIrradiation >>
            return spectra
        
        executeDetection :: DetectionParams -> IO (V.Vector Double, TimeSpec)
        executeDetection DetectionParams{..} =
            enableLightSources [dpIrradiation] >>
            runExceptT (
                ExceptT (setIntegrationTimeMicros spectrometerDeviceID spectrometerFeatureID (floor $ dpExposureTime * 1.0e6)) >>
                ExceptT (measureAveragedSpectrum spectrometerDeviceID spectrometerFeatureID dpNSpectraToAverage)) >>= \spectrum ->
            getTime Monotonic >>= \timeStamp ->
            disableLightSources [dpIrradiation] >>
            case spectrum of
                Left e -> error e
                Right v -> return (v, timeStamp)
        
        spectrometerDeviceID = fst (peSpectrometer env)
        spectrometerFeatureID = snd (peSpectrometer env)
        
        enableLightSources :: [IrradiationParams] -> IO ()
        enableLightSources = mapM_ (\(IrradiationParams sourceName channel power) -> activateLightSource (lookupLightSourceByName sourceName) channel power)
        disableLightSources :: [IrradiationParams] -> IO ()
        disableLightSources = mapM_ (\(IrradiationParams sourceName _ _) -> deactivateLightSource (lookupLightSourceByName sourceName))
