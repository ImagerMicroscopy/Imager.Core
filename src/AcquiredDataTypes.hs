{-# LANGUAGE BangPatterns, DeriveGeneric, DeriveAnyClass #-}
module AcquiredDataTypes
where

import Control.DeepSeq
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Word
import GHC.Generics (Generic)
import System.Clock

import EquipmentTypes
import MiscUtils

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

instance ToJSON AcquiredData where
    toJSON (AcquiredData nRows nCols timeStamp bytes numType) =
        object ["nrows" .= nRows, "ncols" .= nCols, "data" .= (show bytes), "timestamp" .= (timeSpecAsDouble timeStamp), "numtype" .= (show numType)]
    toEncoding (AcquiredData nRows nCols timeStamp bytes numType) =
        pairs ("nrows" .= nRows <> "ncols" .= nCols <> "timestamp" .= (timeSpecAsDouble timeStamp) <> "data" .= (show bytes) <> "numtype" .= (show numType))

data AcquisitionMetaData = AcquisitionMetaData {
                               amdSequence :: !Word64
                             , amdStagePosition :: !StagePosition
                             , amdAcquisitionTypename :: !Text
                           } deriving (Show, Generic, NFData)

instance ToJSON AcquisitionMetaData
instance FromJSON AcquisitionMetaData

instance NFData NumberType where
  rnf t = t `seq` ()
instance NFData TimeSpec where
  rnf t = t `seq` ()
