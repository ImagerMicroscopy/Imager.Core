{-# LANGUAGE BangPatterns, InstanceSigs #-}

module SCCamDetector where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign
import System.Clock

import Detector
import SCCamera
import MiscUtils

data SCCamDetector = SCCamDetector {
                         sccCamName :: !Text
                     }

instance Detector SCCamDetector where
    acquireData :: SCCamDetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO (Either String AcquiredData)
    acquireData (SCCamDetector camName) expTime emGain _ =
        runExceptT (
            ExceptT (setExposureTime camName expTime) >>
            ExceptT (setEMGain camName emGain) >>
            ExceptT (acquireImages camName 1) >>= \im ->
            ExceptT (return $ Right im)
        ) >>= \images ->
        case images of
            Left e  -> return (Left e)
            Right (MeasuredImages nRows nCols vec) ->
                getTime Monotonic >>= \timeStamp ->
                let bytes = byteStringFromVector vec
                    numType = UINT16
                in return $ Right (AcquiredData nRows nCols timeStamp bytes numType)

    acquireStreamingData :: SCCamDetector -> ExposureTime -> Gain -> NMeasurementsToAverage ->
                            NMeasurementsToPerform -> TimeSpec -> MVar [[AcquiredData]] -> IO ()
    acquireStreamingData (SCCamDetector camName) expTime gain nMeasurementsToAverage nMeasurements startTime dataMVar =
        (flip finally) (abortAsyncAcquisition camName) (
            startAsyncAcquisition camName nImagesInBuffer >>= \status ->
            case status of
                Left e -> error e
                Right buffer ->
                    fetchImages (-1) nMeasurements buffer dataMVar
        )
        where
            fetchImages :: Int -> Int -> ImageBuffer -> MVar [[AcquiredData]] -> IO ()
            fetchImages previousIndex nImagesRemaining buffer dataMVar
                | nImagesRemaining == 0 = return ()
                | otherwise =
                    getIndexOfLastImageAsyncAcquired camName >>= \ret ->
                    case ret of
                        Left e -> error e
                        Right i ->
                            if (i == previousIndex)
                            then threadDelay 5000 >> fetchImages previousIndex nImagesRemaining buffer dataMVar
                            else
                                 getTime Monotonic >>= \timeStamp ->
                                 getImageAtIndexInBuffer buffer i >>= \images ->
                                 return (measuredImagesAsAcquiredData images timeStamp) >>= \dat ->
                                 addDataToMVar dataMVar startTime [dat] >>
                                 fetchImages i (nImagesRemaining - 1) buffer dataMVar
            measuredImagesAsAcquiredData :: MeasuredImages -> TimeSpec -> AcquiredData
            measuredImagesAsAcquiredData (MeasuredImages nRows nCols vec) timeStamp =
                AcquiredData nRows nCols timeStamp (byteStringFromVector vec) UINT16
            nImagesInBuffer = 20
    setDetectorTemperature :: SCCamDetector -> Temperature -> IO (Either String ())
    setDetectorTemperature (SCCamDetector camName) t = setTemperature camName t
    getDetectorTemperature :: SCCamDetector -> IO (Either String Temperature)
    getDetectorTemperature (SCCamDetector camName) = readTemperature camName
    getDetectorTemperatureSetpoint :: SCCamDetector -> IO (Either String Temperature)
    getDetectorTemperatureSetpoint (SCCamDetector camName) = readTemperatureSetpoint camName

    getGainRange :: SCCamDetector -> IO (Either String (Gain, Gain))
    getGainRange (SCCamDetector camName) = readEMGainRange camName

    getExposureTimeRange :: SCCamDetector -> IO (Either String (ExposureTime, ExposureTime))
    getExposureTimeRange (SCCamDetector camName) = readExposureTimeRange camName
