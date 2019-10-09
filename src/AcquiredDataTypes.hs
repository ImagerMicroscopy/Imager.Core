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

import Equipment.EquipmentTypes
import Utils.MiscUtils

data NumberType = UINT8
                | UINT16
                | FP64
                deriving (Show, Eq)

data AcquiredData = AcquiredData {
                        acqNRows :: !Int
                      , acqNCols :: !Int
                      , acqTimeStamp :: !TimeSpec
                      , acqDetectorName :: !Text
                      , acqData :: !ByteString
                      , acqNumType :: !NumberType
                  } deriving (Show, Generic, NFData)

instance ToJSON AcquiredData where
    toJSON (AcquiredData nRows nCols timeStamp camName bytes numType) =
        object ["nrows" .= nRows, "ncols" .= nCols, "timestamp" .= (timeSpecAsDouble timeStamp), "detectorname" .= camName, "data" .= (show bytes), "numtype" .= (show numType)]
    toEncoding (AcquiredData nRows nCols timeStamp camName bytes numType) =
        pairs ("nrows" .= nRows <> "ncols" .= nCols <> "timestamp" .= (timeSpecAsDouble timeStamp) <> "detectorname" .= camName <> "data" .= (show bytes) <> "numtype" .= (show numType))

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
