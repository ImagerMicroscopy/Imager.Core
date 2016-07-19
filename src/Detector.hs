{-# LANGUAGE BangPatterns #-}

module Detector where

import Data.ByteString (ByteString)

type ExposureTime = Double
type Gain = Double
type NMeasurementsToAverage = Int

data NumberType = UINT16
                  FP64

data AcquiredData = AcquiredData {
                        acqNRows :: !Int
                      , acqNCols :: !Int
                      , acqData :: !ByteString
                      , acqNumType :: !NumberType
                  } deriving (Show)

class Detector a where
    acquireData :: a -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO AcquiredData
