{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Detector (
    AcquiredData (..)
  , NumberType (..)
  , Detector (..)
  , DetectorParameters (..)
  , DetectorLimits (..)
  , AsyncData (..)
  , ImageOrientationOperation (..)
  , ExposureTime
  , Gain
  , Temperature
  , NMeasurementsToAverage
  , NMeasurementsToPerform
  , getDetectorParameters
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

data DetectorParameters = DetectorParameters {
                              dpDataDimensions :: !(Int, Int)
                            , dpAllowedCropSizes :: ![(Int, Int)]
                            , dpAllowedBinningFactors :: ![Int]
                            , dpCurrentBinFactor :: !Int
                            , dpDetectorLimits :: !DetectorLimits
                            , dpTemperatureSetPoint :: !Double
                          } deriving (Show)

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

data ImageOrientationOperation = IPORotateCW
                               | IPORotateCCW
                               | IPOFlipHorizontal
                               | IPOFlipVertical
                               deriving (Read, Show)

class Detector a where
    acquireData :: a -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO AcquiredData
    acquireStreamingData :: a -> ExposureTime -> Gain -> NMeasurementsToAverage ->
                            NMeasurementsToPerform -> Chan AsyncData -> IO ()
    acquireStreamingData det expTime gain nMeasurementsToAverage nMeasurements chan =
        (acquire chan >> writeChan chan AsyncFinished) `onException` (writeChan chan AsyncError)
        where
            acquire chan = forM_ [1 .. nMeasurements] (\_ ->
                               acquireData det expTime gain nMeasurementsToAverage >>= \acqData ->
                               writeChan chan (AsyncData acqData))

    getDetectorWavelengths :: a -> IO (Vector Double)
    getDetectorWavelengths _ = return V.empty

    setImageOrientation :: a -> [ImageOrientationOperation] -> IO ()
    setImageOrientation _ _ = pure ()
    getDataDimensions :: a -> IO (Int, Int)
    getAllowedCropSizes :: a -> IO [(Int, Int)]
    getAllowedCropSizes det = getDataDimensions det >>= \dim -> pure [dim]
    setCropSize :: a -> (Int, Int) -> IO ()
    setCropSize _ _ = pure ()
    getAllowedBinningFactors :: a -> IO [Int]
    getAllowedBinningFactors det = pure [1]
    setBinningFactor :: a -> Int -> IO ()
    setBinningFactor _ _ = pure ()
    getBinningFactor :: a -> IO Int
    getBinningFactor _ = pure 1

    setDetectorTemperature :: a -> Temperature -> IO ()
    setDetectorTemperature _ _ = return ()
    getDetectorTemperature :: a -> IO Temperature
    getDetectorTemperature _ = return 20.0
    getDetectorTemperatureSetpoint :: a -> IO Temperature
    getDetectorTemperatureSetpoint _ = return 20.0

    getGainRange :: a -> IO (Gain, Gain)
    getGainRange _ = return (1.0, 1.0)
    getExposureTimeRange :: a -> IO (ExposureTime, ExposureTime)

getDetectorParameters :: (Detector a) => a -> IO DetectorParameters
getDetectorParameters det =
    getDataDimensions det >>= \dataDimensions ->
    getAllowedCropSizes det >>= \cropSizes ->
    getAllowedBinningFactors det >>= \binFactors ->
    getBinningFactor det >>= \binFactor ->
    getDetectorTemperatureSetpoint det >>= \tempSetPoint ->
    getGainRange det >>= \(minGain, maxGain) ->
    getExposureTimeRange det >>= \(minExpTime, maxExpTime) ->
    let dls = DetectorLimits minExpTime maxExpTime minGain maxGain 1 100
    in  pure (DetectorParameters dataDimensions cropSizes binFactors binFactor dls tempSetPoint)
