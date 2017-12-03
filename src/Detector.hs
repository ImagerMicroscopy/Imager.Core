{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Detector (
    AcquiredData (..)
  , NumberType (..)
  , Detector (..)
  , DetectorLimits (..)
  , ExposureTime
  , Gain
  , Temperature
  , NMeasurementsToAverage
  , NMeasurementsToPerform
  , getDetectorLimits
  , addDataToMVar
) where

import Control.Concurrent
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

class Detector a where
    acquireData :: a -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO AcquiredData
    acquireStreamingData :: a -> ExposureTime -> Gain -> NMeasurementsToAverage ->
                            NMeasurementsToPerform -> TimeSpec -> MVar [[AcquiredData]] -> IO ()
    acquireStreamingData det expTime gain nMeasurementsToAverage nMeasurements startTime dataMVar =
        forM_ [1 .. nMeasurements] (\_ ->
            acquireData det expTime gain nMeasurementsToAverage >>= \acqData ->
            addDataToMVar dataMVar startTime [acqData])

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

addDataToMVar :: MVar [[AcquiredData]] -> TimeSpec -> [AcquiredData] -> IO ()
addDataToMVar mvar startTime newData =
    newData `deepseq` (return ()) >>
    modifyMVar_ mvar (\previousData ->
        if (null previousData)
        then return [[d] | d <- adjustedData]
        else let nDetectionsAlreadyStored = length (head previousData)
             in when (nDetectionsAlreadyStored > 250) (putStrLn "aborting due to data overflow" >> throwIO (userError "too many async data stored")) >>
                return (zipWith (:) adjustedData previousData))
    where
        adjustedData = map toSecondsFromStart newData
        toSecondsFromStart acqDat =
            let t = acqTimeStamp acqDat
            in acqDat {acqTimeStamp = diffTimeSpec t startTime}
