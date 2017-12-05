{-# LANGUAGE BangPatterns, DeriveGeneric, DeriveAnyClass #-}
module AcquiredDataTypes
where

import Control.DeepSeq
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import System.Clock

data NumberType = UINT8
                | UINT16
                | FP64
                deriving (Show, Eq)

data AcquiredData = AcquiredData {
                        acqNRows :: !Int
                      , acqNCols :: !Int
                      , acqTimeStamp :: !TimeSpec
                      , acqData :: !ByteString
                      , acqNumType :: !NumberType
                  } deriving (Show, Generic, NFData)

instance NFData NumberType where
  rnf t = t `seq` ()
instance NFData TimeSpec where
  rnf t = t `seq` ()
