{-# LANGUAGE BangPatterns, InstanceSigs #-}

module OODetector where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign
import System.Clock
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Detector
import OOSeaBreeze
import MiscUtils
import GPIO

data OODetector = OODetector {
                      oodDeviceID :: !DeviceID
                    , oodFeatureID :: !FeatureID
                    , oodProcessingFeatureID :: !ProcessingFeatureID
                    , oodTriggerParams :: Maybe (GPIOPin, GPIOHandles)
                    , oodNonlinearityCorrection :: Double -> Double
                }

instance Detector OODetector where
    acquireData :: OODetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO (Either String AcquiredData)
    acquireData det expTime gain nSpectraToAverage = acquireData' det expTime gain nSpectraToAverage True

    acquireStreamingData det expTime gain nMeasurementsToAverage nMeasurements startTime dataMVar =
        forM_ [1 .. nMeasurements] (\_ ->
           acquireData' det expTime gain nMeasurementsToAverage False >>= \dat ->
           case dat of
             Left e -> error e
             Right acqData -> addDataToMVar dataMVar startTime [acqData])

    getDetectorWavelengths :: OODetector -> IO (Either String (Vector Double))
    getDetectorWavelengths (OODetector dID fID _ _ _) = getWavelengths dID fID

    getExposureTimeRange :: OODetector -> IO (Either String (ExposureTime, ExposureTime))
    getExposureTimeRange _ = return $ Right (3.8e-3, 1.0)

acquireData' :: OODetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> Bool -> IO (Either String AcquiredData)
acquireData' (OODetector dID fID pfID maybeTrigg corrFunc) expTime _ nSpectraToAverage discardFirstSpectrum =
  (if (isJust maybeTrigg)
   then acquireTriggeredSpectrum (dID, fID, pfID) (fromJust maybeTrigg) expTime nSpectraToAverage
   else acquireSpectrum (dID, fID, pfID) expTime nSpectraToAverage discardFirstSpectrum) >>= \spectrum ->
  case spectrum of
      Left e  -> return (Left e)
      Right v -> return (V.map corrFunc v) >>= \corrected ->
                 getTime Monotonic >>= \timeStamp ->
                 let nRows = V.length corrected
                     nCols = 1
                     bytes = byteStringFromVector corrected
                     numType = FP64
                 in return $ Right (AcquiredData nRows nCols timeStamp bytes numType)

acquireSpectrum :: (DeviceID, FeatureID, ProcessingFeatureID) -> Double -> Int -> Bool -> IO (Either String (Vector Double))
acquireSpectrum (deviceID, featureID, pfID) exposure nSpectra discardFirstSpectrum =
    if ((exposure <= 0.0) || (exposure > 1.0) || (nSpectra < 1))
        then return (Left "invalid number of spectra or exposure time")
        else
          runExceptT (
              ExceptT (setIntegrationTimeMicros deviceID featureID integrationMicroseconds) >>
              ExceptT (if discardFirstSpectrum then measureSpectrum deviceID featureID else return (Right V.empty)) >>   -- force a fresh acquisition if needed
              ExceptT (measureAveragedSpectrum deviceID featureID pfID nSpectra))
    where
        integrationMicroseconds = floor (exposure * 1e6)

acquireTriggeredSpectrum :: (DeviceID, FeatureID, ProcessingFeatureID) -> (GPIOPin, GPIOHandles) -> Double -> Int -> IO (Either String (Vector Double))
acquireTriggeredSpectrum (dID, fID, pfID) (pin, pinH) exposure nSpectra =
    setIntegrationTimeMicros dID fID integrationMicroseconds >>
    (flip finally) (setPinLevel pinH pin Low) (
        setPinLevel pinH pin High >>
        measureAveragedSpectrum dID fID pfID nSpectra)
    where
        integrationMicroseconds = floor (exposure * 1e6)
