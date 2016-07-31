{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Detector where

import Control.Concurrent
import Control.Monad
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
    getGainRange :: a -> IO (Either String (ExposureTime, ExposureTime))
    getExposureTimeRange :: a -> IO (Either String (ExposureTime, ExposureTime))


addDataToMVar :: MVar [[AcquiredData]] -> TimeSpec -> [AcquiredData] -> IO ()
addDataToMVar mvar startTime newData =
    modifyMVar_ mvar (\previousData ->
        when (length previousData > 100) (error "too many async data stored") >>
        return (zipWith (:) (map toSecondsFromStart newData) previousData))
    where
        toSecondsFromStart acqDat =
            let t = acqTimeStamp acqDat
            in acqDat {acqTimeStamp = diffTimeSpec t startTime}
