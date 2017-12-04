{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Detector (
    AcquiredData (..)
  , NumberType (..)
  , Detector (..)
  , DetectorLimits (..)
  , AsyncData (..)
  , ExposureTime
  , Gain
  , Temperature
  , NMeasurementsToAverage
  , NMeasurementsToPerform
  , getDetectorLimits
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

type ExposureTime = Double
type Gain = Double
type Temperature = Double
type NMeasurementsToAverage = Int
type NMeasurementsToPerform = Int

data DetectorLimits = DetectorLimits {
                          dlMinExposureTime :: !ExposureTime
                        , dlMaxExposureTime :: !ExposureTime
                        , dlMinGain :: !Gain
                        , dlMaxGain :: !Gain
                        , dlMinAveraging :: !Int
                        , dlMaxAveraging :: !Int
                    } deriving (Show)

data AsyncData = AsyncData !AcquiredData
               | AsyncFinished
               | AsyncError

class Detector a where
    acquireData :: a -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO AcquiredData
    acquireStreamingData :: a -> ExposureTime -> Gain -> NMeasurementsToAverage ->
                            NMeasurementsToPerform -> IO (Async (), Chan AsyncData)
    acquireStreamingData det expTime gain nMeasurementsToAverage nMeasurements =
        newChan >>= \chan ->
        async ((acquire chan >> writeChan chan AsyncFinished) `onException` writeChan chan AsyncError) >>= \as ->
        return (as, chan)
        where
            acquire chan = forM_ [1 .. nMeasurements] (\_ ->
                               acquireData det expTime gain nMeasurementsToAverage >>= \acqData ->
                               writeChan chan (AsyncData acqData))

    getDetectorWavelengths :: a -> IO (Vector Double)
    getDetectorWavelengths _ = return V.empty

    setDetectorTemperature :: a -> Temperature -> IO ()
    setDetectorTemperature _ _ = return ()
    getDetectorTemperature :: a -> IO Temperature
    getDetectorTemperature _ = return 20.0
    getDetectorTemperatureSetpoint :: a -> IO Temperature
    getDetectorTemperatureSetpoint _ = return 20.0

    getGainRange :: a -> IO (Gain, Gain)
    getGainRange _ = return (1.0, 1.0)
    getExposureTimeRange :: a -> IO (ExposureTime, ExposureTime)

getDetectorLimits :: (Detector a) => a -> IO DetectorLimits
getDetectorLimits det =
    getGainRange det >>= \(minGain, maxGain) ->
    getExposureTimeRange det >>= \(minExpTime, maxExpTime) ->
    return (DetectorLimits minExpTime maxExpTime minGain maxGain 1 100)
