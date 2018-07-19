{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Detector (
    AcquiredData (..)
  , NumberType (..)
  , Detector (..)
  , CameraProperty
  , AsyncData (..)
  , ImageOrientationOperation (..)
  , NMeasurementsToPerform
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import System.Clock
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import AcquiredDataTypes
import SCCamera
import SCCameraTypes

type ExposureTime = Double
type Gain = Double
type Temperature = Double
type NMeasurementsToAverage = Int
type NMeasurementsToPerform = Int

data AsyncData = AsyncData !AcquiredData
               | AsyncFinished
               | AsyncError

data ImageOrientationOperation = IPORotateCW
                               | IPORotateCCW
                               | IPOFlipHorizontal
                               | IPOFlipVertical
                               deriving (Read, Show)

class Detector a where
    acquireData :: a -> IO AcquiredData
    acquireStreamingData :: a -> NMeasurementsToPerform -> Chan AsyncData -> IO ()
    acquireStreamingData det nMeasurements chan =
        (acquire chan >> writeChan chan AsyncFinished) `onException` (writeChan chan AsyncError)
        where
            acquire chan = forM_ [1 .. nMeasurements] (\_ ->
                               acquireData det >>= \acqData ->
                               writeChan chan (AsyncData acqData))

    getDetectorOptions :: a -> IO [CameraProperty]
    setDetectorOption :: a -> CameraProperty -> IO ()
    getDetectorFrameRate :: a -> IO Double
    getDetectorWavelengths :: a -> IO (Vector Double)
    getDetectorWavelengths _ = return V.empty
    setImageOrientation :: a -> [ImageOrientationOperation] -> IO ()
    setImageOrientation _ _ = pure ()
