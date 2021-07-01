{-# LANGUAGE BangPatterns, InstanceSigs #-}

module Detectors.OODetector where

import Control.DeepSeq
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

import Detectors.Detector
import OOSeaBreeze
import Utils.MiscUtils
import GPIO

data OODetector = OODetector {
                      oodDeviceID :: !DeviceID
                    , oodFeatureID :: !FeatureID
                    , oodProcessingFeatureID :: Maybe ProcessingFeatureID
                    , oodTriggerParams :: Maybe (GPIOPin, GPIOHandles)
                    , oodNonlinearityCorrection :: Double -> Double
                }

instance Detector OODetector where
    acquireData :: OODetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO AcquiredData
    acquireData det expTime gain nSpectraToAverage = acquireData' det expTime gain nSpectraToAverage True

    acquireStreamingData det expTime gain nMeasurementsToAverage nMeasurements startTime dataMVar =
        forM_ [1 .. nMeasurements] (\_ ->
           acquireData' det expTime gain nMeasurementsToAverage False >>= \acqData ->
           addDataToMVar dataMVar startTime [acqData])

    getDetectorWavelengths :: OODetector -> IO (Vector Double)
    getDetectorWavelengths (OODetector dID fID _ _ _) = getWavelengths dID fID

    getExposureTimeRange :: OODetector -> IO (ExposureTime, ExposureTime)
    getExposureTimeRange _ = return (3.8e-3, 1.0)

acquireData' :: OODetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> Bool -> IO AcquiredData
acquireData' (OODetector dID fID pfID maybeTrigg corrFunc) expTime _ nSpectraToAverage discardFirstSpectrum =
  (if (isJust maybeTrigg)
   then acquireTriggeredSpectrum (dID, fID, pfID) (fromJust maybeTrigg) expTime nSpectraToAverage
   else acquireSpectrum (dID, fID, pfID) expTime nSpectraToAverage discardFirstSpectrum) >>= return . V.map corrFunc >>= \corrected ->
        getTime Monotonic >>= \timeStamp ->
        let nRows = V.length corrected
            nCols = 1
            bytes = byteStringFromVector corrected
            numType = FP64
            dat = AcquiredData nRows nCols timeStamp bytes numType
        in dat `deepseq` (return dat)

acquireSpectrum :: (DeviceID, FeatureID, Maybe ProcessingFeatureID) -> Double -> Int -> Bool -> IO (Vector Double)
acquireSpectrum (deviceID, featureID, pfID) exposure nSpectra discardFirstSpectrum =
    if ((exposure <= 0.0) || (exposure > 1.0) || (nSpectra < 1))
        then throwIO (userError "invalid number of spectra or exposure time")
        else setIntegrationTimeMicros deviceID featureID integrationMicroseconds >>
             when (discardFirstSpectrum) (measureSpectrum deviceID featureID >> return ()) >>   -- force a fresh acquisition if needed
             measureAveragedSpectrum deviceID featureID pfID nSpectra
    where
        integrationMicroseconds = floor (exposure * 1e6)

acquireTriggeredSpectrum :: (DeviceID, FeatureID, Maybe ProcessingFeatureID) -> (GPIOPin, GPIOHandles) -> Double -> Int -> IO (Vector Double)
acquireTriggeredSpectrum (dID, fID, pfID) (pin, pinH) exposure nSpectra =
    setIntegrationTimeMicros dID fID integrationMicroseconds >>
    (flip finally) (setPinLevel pinH pin Low) (
        setPinLevel pinH pin High >>
        measureAveragedSpectrum dID fID pfID nSpectra)
    where
        integrationMicroseconds = floor (exposure * 1e6)
