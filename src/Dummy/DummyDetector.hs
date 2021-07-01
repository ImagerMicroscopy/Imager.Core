{-# LANGUAGE InstanceSigs #-}
module Dummy.DummyDetector where

import Control.Concurrent
import qualified Data.ByteString as B
import Data.Word
import System.Clock
import System.Random

import Detectors.Detector

data DummyDetector = DummyDetector

instance Detector DummyDetector where
    acquireData :: DummyDetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO AcquiredData
    acquireData _ _ _ _ =
        threadDelay 10000 >>
        getTime Monotonic >>= \timeStamp ->
        randoms <$> newStdGen >>= return . B.pack . take (512 * 512) >>= \datas ->
        return (AcquiredData 512 512 timeStamp datas UINT8)

    getExposureTimeRange :: DummyDetector -> IO (ExposureTime, ExposureTime)
    getExposureTimeRange _ = return (30e-3, 30e-3)
