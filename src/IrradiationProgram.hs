{-# LANGUAGE BangPatterns, DeriveGeneric, RecordWildCards #-}

module IrradiationProgram where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Text (Text)
import Data.Time
import qualified Data.Vector.Storable as V

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
                             ipLightSource :: !LightSource
                           , ipLightSourceChannel :: !Text
                           , ipPower :: !Double
                         }

data ProgramEnvironment = ProgramEnvironment {
                              peSpectrometer :: (DeviceID, FeatureID)
                            , peSpectraMVar :: MVar ([[(V.Vector Double, UTCTime)]])
                          }

executeIrradiationProgram :: IrradiationProgram -> ProgramEnvironment -> IO ()
executeIrradiationProgram (IrradiationProgram steps detection) env =
    mapM_ (executeStep env detection) steps
    where
        executeStep :: ProgramEnvironment -> [DetectionParams] -> ProgramStep -> IO ()
        executeStep env detParams ps =
            forM_ (replicate (psNRepeats ps) ps) $ \step ->
                executeSingleIrradiationInStep detParams step >>= \newSpectra ->
                modifyMVar_ (peSpectraMVar env) (\previousSpectra ->
                    return (previousSpectra ++ [newSpectra]))
        
        executeSingleIrradiationInStep :: [DetectionParams] -> ProgramStep -> IO [(V.Vector Double, UTCTime)]
        executeSingleIrradiationInStep detParams ProgramStep{..} =
            enableLightSources psIrradiation >>
            threadDelay (floor $ psIrradiationDuration * 1.0e6) >>
            mapM executeDetection detParams >>= \spectra ->
            disableLightSources psIrradiation >>
            return spectra
        
        executeDetection :: DetectionParams -> IO (V.Vector Double, UTCTime)
        executeDetection DetectionParams{..} =
            enableLightSources [dpIrradiation] >>
            runExceptT (
                ExceptT (setIntegrationTimeMicros spectrometerDeviceID spectrometerFeatureID (floor $ dpExposureTime * 1.0e6)) >>
                ExceptT (measureAveragedSpectrum spectrometerDeviceID spectrometerFeatureID dpNSpectraToAverage)) >>= \spectrum ->
            getCurrentTime >>= \timeStamp ->
            disableLightSources [dpIrradiation] >>
            case spectrum of
                Left e -> error e
                Right v -> return (v, timeStamp)
        
        spectrometerDeviceID = fst (peSpectrometer env)
        spectrometerFeatureID = snd (peSpectrometer env)
        
        enableLightSources :: [IrradiationParams] -> IO ()
        enableLightSources = mapM_ (\(IrradiationParams source channel power) -> activateLightSource source channel power)
        disableLightSources :: [IrradiationParams] -> IO ()
        disableLightSources = mapM_ (\(IrradiationParams source _ _) -> deactivateLightSource source)
