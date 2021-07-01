{-# LANGUAGE BangPatterns, InstanceSigs #-}

module Detectors.SCCamDetector where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe
import Data.Text (Text)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign
import System.Clock
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV

import Detectors.Detector
import qualified Camera.SCCamera as SC
import qualified Camera.SCCameraTypes as SC
import Utils.MiscUtils

data SCCamDetector = SCCamDetector {
                         sccCamName :: !Text
                     }

instance Detector SCCamDetector where
    detectorName = sccCamName
    acquireData :: SCCamDetector -> IO AcquiredData
    acquireData (SCCamDetector camName) =
        SC.acquireSingleImage camName >>= \(SC.MeasuredImages nRows nCols _ vec) ->
        getTime Monotonic >>= \timeStamp ->
        let bytes = byteStringFromVector vec
            numType = UINT16
        in return (AcquiredData nRows nCols timeStamp camName bytes numType)

    acquireStreamingData :: SCCamDetector -> NMeasurementsToPerform -> Signal -> Chan AsyncData -> IO ()
    acquireStreamingData (SCCamDetector camName) nMeasurements hasStarted chan =
        performAcq `onException` (SC.abortAsyncAcquisition camName >> writeChan chan AsyncError)
        where
            performAcq = getTime Monotonic >>= \acqStart ->
                         SC.startBoundedAsyncAcquisition camName (fromIntegral nMeasurements) >>
                         raiseSignal hasStarted >>
                         fetchImages nMeasurements acqStart chan >>
                         writeChan chan AsyncFinished
            fetchImages :: Int -> TimeSpec -> Chan AsyncData -> IO ()
            fetchImages nImagesRemaining acqStart chan
                | nImagesRemaining == 0 = return ()
                | otherwise =
                    fetchNextImage >>= \(SC.MeasuredImages nRows nCols timeStamp imageData) ->
                    let shiftedTimeStamp = fromNanoSecs (toNanoSecs acqStart + round (timeStamp * 1.0e9))
                        acqData = AcquiredData nRows nCols shiftedTimeStamp camName (byteStringFromVector imageData) UINT16
                    in  acqData `deepseq` writeChan chan (AsyncData acqData) >>
                        fetchImages (nImagesRemaining - 1) acqStart chan
            fetchNextImage = SC.getNextAcquiredImage camName 500 >>= \maybeImg ->
                             if (isJust maybeImg)
                                 then return (fromJust maybeImg)
                                 else fetchNextImage

    getDetectorProperties :: SCCamDetector -> IO [DetectorProperty]
    getDetectorProperties (SCCamDetector camName) = SC.getCameraOptions camName
    setDetectorOption :: SCCamDetector -> DetectorProperty -> IO ()
    setDetectorOption (SCCamDetector camName) opt = SC.setCameraOption camName opt
    getDetectorFrameRate :: SCCamDetector -> IO Double
    getDetectorFrameRate (SCCamDetector camName) = SC.getFrameRate camName
    isConfiguredForHardwareTriggering (SCCamDetector camName) = SC.isConfiguredForHardwareTriggering camName

    setImageOrientation :: SCCamDetector -> [ImageOrientationOperation] -> IO ()
    setImageOrientation (SCCamDetector camName) ops =
        SC.setCameraOrientation camName (map toSC ops)
        where
            toSC :: ImageOrientationOperation -> SC.OrientationOp
            toSC IPORotateCW = SC.RotateCWOp
            toSC IPORotateCCW = SC.RotateCCWOp
            toSC IPOFlipHorizontal = SC.FlipHorizontalOp
            toSC IPOFlipVertical = SC.FlipVerticalOp
