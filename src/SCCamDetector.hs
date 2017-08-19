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
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV

import Detector
import SCCamera
import MiscUtils

data SCCamDetector = SCCamDetector {
                         sccCamName :: !Text
                     }

instance Detector SCCamDetector where
    acquireData :: SCCamDetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO AcquiredData
    acquireData (SCCamDetector camName) expTime emGain nAvg =
        setExposureTime camName expTime >>
        setEMGain camName emGain >>
        acquireImages camName 1 nAvg >>= \(MeasuredImages nRows nCols vec) ->
        getTime Monotonic >>= \timeStamp ->
        let bytes = byteStringFromVector vec
            numType = UINT16
        in return (AcquiredData nRows nCols timeStamp bytes numType)

    acquireStreamingData :: SCCamDetector -> ExposureTime -> Gain -> NMeasurementsToAverage ->
                            NMeasurementsToPerform -> TimeSpec -> MVar [[AcquiredData]] -> IO ()
    acquireStreamingData (SCCamDetector camName) expTime gain nAvg nMeasurements startTime dataMVar =
        getSensorDimensions camName >>= \(nRows, nCols) ->
        MV.new (nRows * nCols * nImagesInBuffer * 2) >>= \buffer ->
        MV.unsafeWith buffer (\bufPtr ->
        (flip finally) (abortAsyncAcquisition camName) (
            setExposureTime camName expTime >>
            setEMGain camName gain >>
            startAsyncAcquisition camName nAvg (bufPtr, nRows * nCols * nImagesInBuffer) >>
            fetchImages nMeasurements (bufPtr, nRows, nCols) dataMVar))
        where
            fetchImages :: Int -> (Ptr Word16, Int, Int) -> MVar [[AcquiredData]] -> IO ()
            fetchImages nImagesRemaining (bufPtr, nRows, nCols) dataMVar
                | nImagesRemaining == 0 = return ()
                | otherwise =
                    getIndexOfLastImageAsyncAcquired camName >>= \idx ->
                    case idx of
                        (-1) -> threadDelay (floor 50e3) >> fetchImages nImagesRemaining (bufPtr, nRows, nCols) dataMVar
                        i    -> getTime Monotonic >>= \timeStamp ->
                                getImageAtIndexInBuffer bufPtr (nRows, nCols) i >>= \imageData ->
                                addDataToMVar dataMVar startTime [AcquiredData nRows nCols timeStamp (byteStringFromVector imageData) UINT16] >>
                                fetchImages (nImagesRemaining - 1) (bufPtr, nRows, nCols) dataMVar
            nImagesInBuffer = 20
            getImageAtIndexInBuffer :: Ptr Word16 -> (Int, Int) -> Int -> IO (V.Vector Word16)
            getImageAtIndexInBuffer bufPtr (nRows, nCols) index =
                let nBytesInImage = nRows * nCols * 2
                    offsetPtr = bufPtr `plusPtr` (index * nBytesInImage)
                in  MV.new (nRows * nCols) >>= \image ->
                    MV.unsafeWith image (\imPtr -> copyBytes imPtr offsetPtr nBytesInImage) >>
                    V.freeze image

    setDetectorTemperature :: SCCamDetector -> Temperature -> IO ()
    setDetectorTemperature (SCCamDetector camName) t = setTemperature camName t
    getDetectorTemperature :: SCCamDetector -> IO Temperature
    getDetectorTemperature (SCCamDetector camName) = readTemperature camName
    getDetectorTemperatureSetpoint :: SCCamDetector -> IO Temperature
    getDetectorTemperatureSetpoint (SCCamDetector camName) = readTemperatureSetpoint camName

    getGainRange :: SCCamDetector -> IO (Gain, Gain)
    getGainRange (SCCamDetector camName) = readEMGainRange camName

    getExposureTimeRange :: SCCamDetector -> IO (ExposureTime, ExposureTime)
    getExposureTimeRange (SCCamDetector camName) = readExposureTimeRange camName
