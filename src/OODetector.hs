{-# LANGUAGE BangPatterns, InstanceSigs #-}

module OODetector where

import Control.Exception
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign
import System.Clock

import Detector
import OOSeaBreeze
import MiscUtils
import GPIO

data OODetector = OODetector {
                      oodDeviceID :: !DeviceID
                    , oodFeatureID :: !FeatureID
                    , oodTriggerParams :: Maybe (GPIOPin, GPIOHandles)
                    , oodNonlinearityCorrection :: Double -> Double
                }

instance Detector OODetector where
    acquireData :: OODetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO (Either String AcquiredData)
    acquireData (OODetector dID fID maybeTrigg corrFunc) expTime _ nSpectraToAverage =
        (if (isJust maybeTrigg)
         then acquireTriggeredSpectrum (dID, fID) (fromJust maybeTrigg) expTime nSpectraToAverage
         else acquireSpectrum (dID, fID) expTime nSpectraToAverage) >>= \spectrum ->
        case spectrum of
            Left e  -> return (Left e)
            Right v -> return (V.map corrFunc v) >>= \corrected ->
                       let nRows = V.length corrected
                           nCols = 1
                           bytes = byteStringFromVector corrected
                           numType = FP64
                       in return $ Right (AcquiredData nRows nCols bytes numType)
    getGainRange :: OODetector -> IO (Either String (Gain, Gain))
    getGainRange _ = return $ Right (1.0, 1.0)
    getExposureTimeRange :: OODetector -> IO (Either String (ExposureTime, ExposureTime))
    getExposureTimeRange _ = return $ Right (3.8e-3, 1.0)

acquireSpectrum :: (DeviceID, FeatureID) -> Double -> Int -> IO (Either String (Vector Double))
acquireSpectrum (deviceID, featureID) exposure nSpectra =
    if ((exposure <= 0.0) || (exposure > 1.0) || (nSpectra < 1))
        then return (Left "invalid number of spectra or exposure time")
        else
          runExceptT (
              ExceptT (setIntegrationTimeMicros deviceID featureID integrationMicroseconds) >>
              ExceptT (measureSpectrum deviceID featureID) >>   -- force a fresh acquisition
              ExceptT (measureAveragedSpectrum deviceID featureID nSpectra))
    where
        integrationMicroseconds = floor (exposure * 1e6)

acquireTriggeredSpectrum :: (DeviceID, FeatureID) -> (GPIOPin, GPIOHandles) -> Double -> Int -> IO (Either String (Vector Double))
acquireTriggeredSpectrum (dID, fID) (pin, pinH) exposure nSpectra =
    setIntegrationTimeMicros dID fID integrationMicroseconds >>
    (flip finally) (setPinLevel pinH pin Low) (
        setPinLevel pinH pin High >>
        measureAveragedSpectrum dID fID nSpectra)
    where
        integrationMicroseconds = floor (exposure * 1e6)

acquireWavelengths :: (DeviceID, FeatureID) -> IO (Either String (Vector Double))
acquireWavelengths (deviceID, featureID) =
    getWavelengths deviceID featureID

fetchAvailableSpectrometer :: IO (Maybe (DeviceID, FeatureID))
fetchAvailableSpectrometer =
    getDeviceIDs >>= \idList ->
    if (not $ null idList)
      then
        runExceptT (
            ExceptT (openDevice (head idList)) >>
            ExceptT (getSpectrometerFeatures (head idList)) >>= \(featureID : _) ->
            return (head idList, featureID)) >>= \result ->
        case result of
            Left _  -> return Nothing
            Right v -> return $ Just v
      else return Nothing

closeAvailableSpectrometer :: Maybe (DeviceID, FeatureID) -> IO ()
closeAvailableSpectrometer Nothing              = return ()
closeAvailableSpectrometer (Just (deviceID, _)) = closeDevice deviceID >> return ()
