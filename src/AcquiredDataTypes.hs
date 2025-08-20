{-# LANGUAGE BangPatterns, DeriveGeneric, DeriveAnyClass #-}
module AcquiredDataTypes
where

import Control.Concurrent
import Control.DeepSeq
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.MessagePack
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Word
import Debug.Trace
import GHC.Generics (Generic)
import System.Clock

import Equipment.EquipmentTypes
import Utils.MiscUtils

data MessageChannel = MessageChannel (MVar [AsyncMeasurementMessage])

newMessageChannel :: IO MessageChannel
newMessageChannel = MessageChannel <$> newMVar []

sendMessageToChannel :: MessageChannel -> AsyncMeasurementMessage -> IO ()
sendMessageToChannel (MessageChannel mvar) msg =
    modifyMVar_ mvar (\msgs -> pure (msg : msgs))

readChannelMessages :: MessageChannel -> IO [AsyncMeasurementMessage]
readChannelMessages (MessageChannel mvar) = reverse <$> readMVar mvar

deleteChannelMessagesUpToIndex :: MessageChannel -> Word64 -> IO ()
deleteChannelMessagesUpToIndex (MessageChannel mvar) idx =
    modifyMVar_ mvar (\msgs ->
        pure $ filter (\msg -> asyncMessageIndex msg > idx) msgs)

numMessagesInChannel :: MessageChannel -> IO Int
numMessagesInChannel (MessageChannel mvar) =
    length <$> readMVar mvar

data AsyncMeasurementMessage = AcquiredDataMessage {
                                   messageIdx :: Word64
                                 , acquisitionMetaData :: AcquisitionMetaData
                                 , acquiredData :: AcquiredData
                               }
                             deriving (Show)

asyncMessageIndex :: AsyncMeasurementMessage -> Word64
asyncMessageIndex = messageIdx

instance MessagePack AsyncMeasurementMessage where
    toObject m@(AcquiredDataMessage{}) =
        toObject $ M.fromList [
                    ("index" :: Text, toObject (fromIntegral m.messageIdx :: Int)),
                    ("metadata", toObject m.acquisitionMetaData),
                    ("data", toObject m.acquiredData),
                    ("type", toObject ("acquireddatamessage" :: Text))
                 ]

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
        object ["nrows" .= nRows, "ncols" .= nCols, "timestamp" .= (timeSpecAsSeconds timeStamp), "detectorname" .= camName, "data" .= (show bytes), "numtype" .= (show numType)]
    toEncoding (AcquiredData nRows nCols timeStamp camName bytes numType) =
        pairs ("nrows" .= nRows <> "ncols" .= nCols <> "timestamp" .= (timeSpecAsSeconds timeStamp) <> "detectorname" .= camName <> "data" .= (show bytes) <> "numtype" .= (show numType))

instance MessagePack AcquiredData where
    toObject d = toObject $ M.fromList [
                             ("nrows" :: Text, toObject d.acqNRows),
                             ("ncols", toObject d.acqNCols),
                             ("timestamp", toObject (timeSpecAsSeconds d.acqTimeStamp)),
                             ("detectorname", toObject d.acqDetectorName),
                             ("imagedata", toObject d.acqData),
                             ("numtype", toObject (encodedNumType d.acqNumType))
                          ]
    fromObject _ = error "no fromObject for AcquiredData"

data AcquisitionMetaData = AcquisitionMetaData {
                               amdStagePosition :: !StagePosition
                             , amdAcquisitionTypename :: !Text
                           } deriving (Show, Generic, NFData)

instance ToJSON AcquisitionMetaData
instance FromJSON AcquisitionMetaData

instance MessagePack AcquisitionMetaData where
    toObject (AcquisitionMetaData position typename) =
      toObject $ M.fromList [
                 ("stageposition" :: Text, toObject position),
                 ("acquisitiontype", toObject typename)
               ]
    fromObject _ = error "no fromObject for AcquisitionMetaData"

instance ToJSON AsyncMeasurementMessage where
  toJSON p@(AcquiredDataMessage{}) = toJSON (p.acquisitionMetaData, p.acquiredData) -- should not be used since this data should be binary encoded instead
  toJSON _                       = error "no ToJSON for this AcquiredDataMessage type"

data NumberType = UINT8
                | UINT16
                | FP64
                deriving (Show, Eq)

encodedNumType :: NumberType -> Int
encodedNumType UINT8 = 2
encodedNumType UINT16 = 0
encodedNumType FP64 = 1

instance NFData NumberType where
  rnf t = t `seq` ()
instance NFData TimeSpec where
  rnf t = t `seq` ()
