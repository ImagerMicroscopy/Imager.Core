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
import qualified SCCamera as SC
import MiscUtils

data SCCamDetector = SCCamDetector {
                         sccCamName :: !Text
                     }

instance Detector SCCamDetector where
    acquireData :: SCCamDetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO AcquiredData
    acquireData (SCCamDetector camName) expTime emGain nAvg =
        SC.setExposureTime camName expTime >>
        SC.setEMGain camName emGain >>
        SC.acquireImages camName 1 nAvg >>= \(SC.MeasuredImages nRows nCols vec) ->
        getTime Monotonic >>= \timeStamp ->
        let bytes = byteStringFromVector vec
            numType = UINT16
        in return (AcquiredData nRows nCols timeStamp bytes numType)

    acquireStreamingData :: SCCamDetector -> ExposureTime -> Gain -> NMeasurementsToAverage ->
                            NMeasurementsToPerform -> Chan AsyncData -> IO ()
    acquireStreamingData (SCCamDetector camName) expTime gain nAvg nMeasurements chan =
        SC.setExposureTime camName expTime >>
        SC.setEMGain camName gain >>
        (performAcq >> SC.abortAsyncAcquisition camName) `onException` (SC.abortAsyncAcquisition camName >>
                                                                                    writeChan chan AsyncError)
        where
            performAcq = SC.startAsyncAcquisition camName nAvg >>
                         fetchImages nMeasurements chan >>
                         writeChan chan AsyncFinished
            fetchImages :: Int -> Chan AsyncData -> IO ()
            fetchImages nImagesRemaining chan
                | nImagesRemaining == 0 = return ()
                | otherwise =
                    SC.getNextAcquiredImage camName >>= \(SC.MeasuredImages nRows nCols imageData) ->
                    getTime Monotonic >>= \timeStamp ->
                    let acqData = AcquiredData nRows nCols timeStamp (byteStringFromVector imageData) UINT16
                    in  acqData `deepseq` writeChan chan (AsyncData acqData) >>
                        fetchImages (nImagesRemaining - 1) chan

    setImageOrientation :: SCCamDetector -> [ImageOrientationOperation] -> IO ()
    setImageOrientation (SCCamDetector camName) ops =
        SC.setCameraOrientation camName (map toSC ops)
        where
            toSC :: ImageOrientationOperation -> SC.OrientationOp
            toSC IPORotateCW = SC.RotateCWOp
            toSC IPORotateCCW = SC.RotateCCWOp
            toSC IPOFlipHorizontal = SC.FlipHorizontalOp
            toSC IPOFlipVertical = SC.FlipVerticalOp

    getDataDimensions :: SCCamDetector -> IO (Int, Int)
    getDataDimensions (SCCamDetector camName) = SC.getImageDimensions camName
    getAllowedCropSizes (SCCamDetector camName) = SC.getAllowedCropSizes camName
    setCropSize (SCCamDetector camName) size = SC.setCropSize camName size
    getAllowedBinningFactors (SCCamDetector camName) = SC.getAllowedBinningFactors camName
    setBinningFactor (SCCamDetector camName) factor = SC.setBinningFactor camName factor
    getBinningFactor (SCCamDetector camName) = SC.getBinningFactor camName

    setDetectorTemperature :: SCCamDetector -> Temperature -> IO ()
    setDetectorTemperature (SCCamDetector camName) t = SC.setTemperature camName t
    getDetectorTemperature :: SCCamDetector -> IO Temperature
    getDetectorTemperature (SCCamDetector camName) = SC.readTemperature camName
    getDetectorTemperatureSetpoint :: SCCamDetector -> IO Temperature
    getDetectorTemperatureSetpoint (SCCamDetector camName) = SC.readTemperatureSetpoint camName

    getGainRange :: SCCamDetector -> IO (Gain, Gain)
    getGainRange (SCCamDetector camName) = SC.readEMGainRange camName

    getExposureTimeRange :: SCCamDetector -> IO (ExposureTime, ExposureTime)
    getExposureTimeRange (SCCamDetector camName) = SC.readExposureTimeRange camName
