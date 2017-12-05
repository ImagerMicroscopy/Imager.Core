{-# LANGUAGE BangPatterns, InstanceSigs #-}

module SCCamDetector where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.DeepSeq
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
import CameraImageProcessing

data SCCamDetector = SCCamDetector {
                         sccCamName :: !Text
                       , sccImageProcessing :: ![ExternalRearrangementFunc]
                     }

data ImageProcessingOperation = IPORotateCW | IPORotateCCW
                              | IPOFlipHorizontal | IPOFlipVertical
                                deriving (Show, Read)

processingFunc :: ImageProcessingOperation -> ExternalRearrangementFunc
processingFunc IPORotateCW = cRotateImageCW
processingFunc IPORotateCCW = cRotateImageCCW
processingFunc IPOFlipHorizontal = cFlipImageHorizontal
processingFunc IPOFlipVertical = cFlipImageVertical

-- processAcquiredImages :: [ExternalRearrangementFunc] -> MeasuredImages -> MeasuredImages
-- processAcquiredImages [] ims = ims
-- processAcquiredImages fs (MeasuredImages nRows nCols v) =
--     let nPixels = nRows * nCols
--         nIms = (V.length v) `div` nPixels
--         ims = take nIms (map (\im -> ImageVectorAndSize im (nRows, nCols)) . map (\s -> V.slice 0 nPixels v) $ [0, nPixels ..])
--         rearrangedIms = map (rearrangeImageExternal fs) ims
--         (newNRows, newNCols) = ivsSize (head rearrangedIms)
--         newV = V.concat (map ivsVector rearrangedIms)
--     in  MeasuredImages newNRows newNCols newV--MeasuredImages newNRows newNCols newV

instance Detector SCCamDetector where
    acquireData :: SCCamDetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO AcquiredData
    acquireData (SCCamDetector camName procF) expTime emGain nAvg =
        setExposureTime camName expTime >>
        setEMGain camName emGain >>
        acquireImages camName 1 nAvg >>= \(MeasuredImages nRows nCols vec) ->
        getTime Monotonic >>= \timeStamp ->
        let bytes = byteStringFromVector vec
            numType = UINT16
        in return (AcquiredData nRows nCols timeStamp bytes numType)

    acquireStreamingData :: SCCamDetector -> ExposureTime -> Gain -> NMeasurementsToAverage ->
                            NMeasurementsToPerform -> IO (Async (), Chan AsyncData)
    acquireStreamingData (SCCamDetector camName procF) expTime gain nAvg nMeasurements =
        getSensorDimensions camName >>= \(nRows, nCols) ->
        setExposureTime camName expTime >>
        setEMGain camName gain >>
        newChan >>= \chan ->
        async ((MV.new (nRows * nCols * nImagesInBuffer * 2) >>= \buffer ->
                MV.unsafeWith buffer (\bufPtr ->
                startAsyncAcquisition camName nAvg (bufPtr, nRows * nCols * nImagesInBuffer) >>
                putStrLn "async running" >>
                fetchImages nMeasurements (bufPtr, nRows, nCols) chan) >>
                writeChan chan AsyncFinished)
                   `onException` (writeChan chan AsyncError >>
                                  abortAsyncAcquisition camName)) >>= \as ->
        return (as, chan)
        where
            fetchImages :: Int -> (Ptr Word16, Int, Int) -> Chan AsyncData -> IO ()
            fetchImages nImagesRemaining (bufPtr, nRows, nCols) chan
                | nImagesRemaining == 0 = return ()
                | otherwise =
                    getIndexOfLastImageAsyncAcquired camName >>= \idx ->
                    case idx of
                        (-1) -> threadDelay (floor 50e3) >> fetchImages nImagesRemaining (bufPtr, nRows, nCols) chan
                        i    -> getTime Monotonic >>= \timeStamp ->
                                getImageAtIndexInBuffer bufPtr (nRows, nCols) i >>= \imageData ->
                                let acqData = AcquiredData nRows nCols timeStamp (byteStringFromVector imageData) UINT16
                                in  acqData `deepseq` writeChan chan (AsyncData acqData) >>
                                    fetchImages (nImagesRemaining - 1) (bufPtr, nRows, nCols) chan
            nImagesInBuffer = 20
            getImageAtIndexInBuffer :: Ptr Word16 -> (Int, Int) -> Int -> IO (V.Vector Word16)
            getImageAtIndexInBuffer bufPtr (nRows, nCols) index =
                let nBytesInImage = nRows * nCols * 2
                    offsetPtr = bufPtr `plusPtr` (index * nBytesInImage)
                in  MV.new (nRows * nCols) >>= \image ->
                    MV.unsafeWith image (\imPtr -> copyBytes imPtr offsetPtr nBytesInImage) >>
                    V.freeze image

    setDetectorTemperature :: SCCamDetector -> Temperature -> IO ()
    setDetectorTemperature (SCCamDetector camName _) t = setTemperature camName t
    getDetectorTemperature :: SCCamDetector -> IO Temperature
    getDetectorTemperature (SCCamDetector camName _) = readTemperature camName
    getDetectorTemperatureSetpoint :: SCCamDetector -> IO Temperature
    getDetectorTemperatureSetpoint (SCCamDetector camName _) = readTemperatureSetpoint camName

    getGainRange :: SCCamDetector -> IO (Gain, Gain)
    getGainRange (SCCamDetector camName _) = readEMGainRange camName

    getExposureTimeRange :: SCCamDetector -> IO (ExposureTime, ExposureTime)
    getExposureTimeRange (SCCamDetector camName _) = readExposureTimeRange camName
