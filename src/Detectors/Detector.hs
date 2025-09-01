{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Detectors.Detector (
    AcquiredData (..)
  , NumberType (..)
  , Detector (..)
  , DetectorProperty
  , AsyncData (..)
  , ImageOrientationOperation (..)
  , NMeasurementsToPerform
  , acquireMultipleDetectorStreamingData
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Clock
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Camera.SCCameraTypes
import Measurements.MeasurementProgramTypes
import Utils.MiscUtils

type ExposureTime = Double
type Gain = Double
type Temperature = Double
type NMeasurementsToAverage = Int
type NMeasurementsToPerform = Int

data AsyncData = AsyncData !(Text, MeasuredImage)   -- (detectorName, image)
               | AsyncFinished
               | AsyncError

data ImageOrientationOperation = IPORotateCW
                               | IPORotateCCW
                               | IPOFlipHorizontal
                               | IPOFlipVertical
                               deriving (Read, Show)

class Detector a where
    detectorName :: a -> Text
    acquireData :: a -> IO MeasuredImage
    acquireStreamingData :: a -> NMeasurementsToPerform -> Signal -> Chan AsyncData -> IO ()
    acquireStreamingData det nMeasurements hasStarted chan =    -- default implementation that can be overwritten
        (raiseSignal hasStarted >> acquire chan >> writeChan chan AsyncFinished) `onException` (writeChan chan AsyncError)
        where
            acquire chan = forM_ [1 .. nMeasurements] (\_ ->
                               acquireData det >>= \measuredImage ->
                               writeChan chan (AsyncData (detectorName det, measuredImage)))

    getDetectorProperties :: a -> IO [DetectorProperty]
    setDetectorOption :: a -> DetectorProperty -> IO ()
    getDetectorFrameRate :: a -> IO Double
    isConfiguredForHardwareTriggering :: a -> IO Bool
    getDetectorWavelengths :: a -> IO (Vector Double)
    getDetectorWavelengths _ = return V.empty
    setImageOrientation :: a -> [ImageOrientationOperation] -> IO ()
    setImageOrientation _ _ = pure ()

acquireMultipleDetectorStreamingData :: (Detector a) => [a] -> IO () -> IO () -> NMeasurementsToPerform -> Chan AsyncData -> IO ()
acquireMultipleDetectorStreamingData dets actionBeforeAcquisition actionAfterAcquisition nMeasurements chan =
    doTheWork `onException` (writeChan chan AsyncError)
    where
        doTheWork =
            partitionM isConfiguredForHardwareTriggering dets >>= \(withTrigs, withoutTrigs) ->
            replicateM (length withTrigs) newSignal >>= \trigSignals ->
            withAsync (forConcurrently_ (zip withTrigs trigSignals) (\(det, hasStarted) ->
                        acquireStreamingData det nMeasurements hasStarted chan)) (\firstAs ->
                mapM_ waitForSignal trigSignals >>
                actionBeforeAcquisition >>
                withAsync (forConcurrently_ withoutTrigs (\det -> newSignal >>= \hasStarted ->
                                                                acquireStreamingData det nMeasurements hasStarted chan)) (\secondAs ->
                    wait firstAs >> wait secondAs >>
                    actionAfterAcquisition))
