{-# LANGUAGE InstanceSigs #-}
module DummyDetector where

import qualified Data.ByteString as B
import Data.Word
import System.Clock
import System.Random

import Detector

data DummyDetector = DummyDetector

instance Detector DummyDetector where
    acquireData :: DummyDetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO (Either String AcquiredData)
    acquireData _ _ _ _ =
        getTime Monotonic >>= \timeStamp ->
        randoms <$> newStdGen >>= return . B.pack . take (512 * 512) >>= \datas ->
        return (Right $ AcquiredData 512 512 timeStamp datas UINT8)

    getExposureTimeRange :: DummyDetector -> IO (Either String (ExposureTime, ExposureTime))
    getExposureTimeRange _ = return $ Right (30e-3, 30e-3)
