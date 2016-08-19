{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Detector (
    AcquiredData (..)
  , NumberType (..)
  , Detector (..)
  , DetectorLimits (..)
  , ExposureTime
  , Gain
  , NMeasurementsToAverage
  , NMeasurementsToPerform
  , getDetectorLimits
  , addDataToMVar
) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import System.Clock

type ExposureTime = Double
type Gain = Double
type NMeasurementsToAverage = Int
type NMeasurementsToPerform = Int

data NumberType = UINT16
                | FP64
                deriving (Show)

data AcquiredData = AcquiredData {
                        acqNRows :: !Int
                      , acqNCols :: !Int
                      , acqTimeStamp :: !TimeSpec
                      , acqData :: !ByteString
                      , acqNumType :: !NumberType
                  } deriving (Show)

data DetectorLimits = DetectorLimits {
                          dlMinExposureTime :: !ExposureTime
                        , dlMaxExposureTime :: !ExposureTime
                        , dlMinGain :: !Gain
                        , dlMaxGain :: !Gain
                        , dlMinAveraging :: !Int
                        , dlMaxAveraging :: !Int
                    } deriving (Show)

class Detector a where
    acquireData :: a -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO (Either String AcquiredData)
    acquireStreamingData :: a -> ExposureTime -> Gain -> NMeasurementsToAverage ->
                            NMeasurementsToPerform -> TimeSpec -> MVar [[AcquiredData]] -> IO ()
    acquireStreamingData det expTime gain nMeasurementsToAverage nMeasurements startTime dataMVar =
        forM_ [1 .. nMeasurements] (\_ ->
            acquireData det expTime gain nMeasurementsToAverage >>= \dat ->
            case dat of
              Left e -> error e
              Right acqData -> addDataToMVar dataMVar startTime [acqData])
    getGainRange :: a -> IO (Either String (Gain, Gain))
    getExposureTimeRange :: a -> IO (Either String (ExposureTime, ExposureTime))

getDetectorLimits :: (Detector a) => a -> IO (Either String DetectorLimits)
getDetectorLimits det =
    runExceptT (
        ExceptT (getGainRange det) >>= \(minGain, maxGain) ->
        ExceptT (getExposureTimeRange det) >>= \(minExpTime, maxExpTime) ->
        ExceptT (return $ Right (DetectorLimits minExpTime maxExpTime minGain maxGain 1 100))
    )

addDataToMVar :: MVar [[AcquiredData]] -> TimeSpec -> [AcquiredData] -> IO ()
addDataToMVar mvar startTime newData =
    modifyMVar_ mvar (\previousData ->
        if (null previousData)
        then return [[d] | d <- adjustedData]
        else when (length previousData > 100) (error "too many async data stored") >>
             return (zipWith (:) adjustedData previousData))
    where
        adjustedData = map toSecondsFromStart newData
        toSecondsFromStart acqDat =
            let t = acqTimeStamp acqDat
            in acqDat {acqTimeStamp = diffTimeSpec t startTime}
