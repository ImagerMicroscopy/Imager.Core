{-# LANGUAGE BangPatterns, OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module CuvettorTypes where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Parallel.Strategies
import GHC.Generics
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Unsafe as SB
import qualified Data.ByteString.Base64 as B64
import Data.Char
import Data.MessagePack
import Data.Monoid
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign
import System.Clock
import System.IO.Unsafe

import Camera.SCCameraTypes
import Detectors.Detector
import Equipment.Equipment
import Encodings.EquipmentEncoding
import Equipment.EquipmentTypes
import Measurements.MeasurementProgram
import Measurements.MeasurementProgramTypes
import Utils.MiscUtils
import Utils.SharedMemory

data Environment a = Environment {
                      envEquipment :: ![EquipmentW]
                    , envDetectors :: ![a]
                    , envEncodedSpectrometerWavelengths :: !B.ByteString
                    , envAsyncDataChannel :: !MessageChannel
                    , envAsyncStatusMessagesMVar :: !(MVar [Text])
                    , envAsyncProgramWorker :: !(Async ())
                    , envUseSharedMemoryForTransfer :: Maybe SharedMemory
}

type ExposureTime = Double

data RequestMessage = AcquireData !DetectionParams
                    | ListWavelengths
                    | ListAvailableEquipment
                    | GetMotorizedStagePosition !StageName
                    | SetMotorizedStagePosition !StageName !StagePosition
                    | ListAvailableDetectors
                    | GetDetectorProperties !DetectorName
                    | SetDetectorProperty !DetectorName !DetectorProperty
                    | Ping
                    | ExecuteMeasurementProgram {
                          execMeasurementProgram :: !MeasurementElement
                        , execMeasurementDetections :: !DefinedDetections
                        , execMeasurementSmartProgramCode :: !SmartProgramCode
                      }
                    | FetchAsyncData
                    | UseSharedMemoryForTransfer !Bool
                    | AcknowledgeDataReceipt !Word64
                    | FetchAsyncStatusMessages
                    | CancelAsyncAcquisition
                    | IsAsyncAcquisitionRunning
                    deriving (Generic)

instance ToJSON RequestMessage where
    toEncoding ListWavelengths = pairs ("action" .= ("listwavelengths" :: Text))
    toEncoding (AcquireData p) = pairs ("action" .= ("acquiredata"  :: Text) <> "params" .= p)
    toEncoding ListAvailableEquipment = pairs ("action" .= ("listavailableequipment" :: Text))
    toEncoding (GetMotorizedStagePosition name) = pairs ("action" .= ("getmotorizedstageposition" :: Text) <> "name" .= name)
    toEncoding (SetMotorizedStagePosition name ds) = pairs ("action" .= ("setmotorizedstageposition" :: Text) <> "name" .= name <> "position" .= ds)
    toEncoding ListAvailableDetectors = pairs ("action" .= ("listavailabledetectors" :: Text))
    toEncoding (GetDetectorProperties detName) = pairs ("action" .= ("getdetectorproperties" :: Text) <> "detectorname" .= detName)
    toEncoding (SetDetectorProperty detName prop) = pairs ("action" .= ("setdetectorproperty" :: Text) <> "detectorname" .= detName <> "property" .= prop)
    toEncoding Ping = pairs ("action" .= ("ping" :: Text))
    toEncoding (ExecuteMeasurementProgram prog dets smartprog) = pairs (
        "action" .= ("executemeasurementprogram" :: Text) <>
        "program" .= prog <>
        "defineddetections" .= dets <>
        "smartprogramcode" .= smartprog)
    toEncoding FetchAsyncData = pairs ("action" .= ("fetchasyncspectra" :: Text))
    toEncoding (UseSharedMemoryForTransfer useShMem) = pairs ("action" .= ("usesharedmemoryfortransfer" :: Text) <> "usesharedmemory" .= useShMem)
    toEncoding (AcknowledgeDataReceipt upToIdx) = pairs ("action" .= ("acknowledgedatareceipt" :: Text) <> "uptoandincluding" .= upToIdx)
    toEncoding FetchAsyncStatusMessages = pairs ("action" .= ("fetchasyncstatusmessages" :: Text))
    toEncoding CancelAsyncAcquisition = pairs ("action" .= ("cancelasyncacquisition" :: Text))
    toEncoding IsAsyncAcquisitionRunning = pairs ("action" .= ("isasyncacquisitionrunning" :: Text))

instance FromJSON RequestMessage where
    parseJSON (Object v) =
        v .: "action" >>= \action ->
        case (T.toLower action) of
            "acquiredata" -> AcquireData <$> v .: "params"
            "listwavelengths" -> return ListWavelengths
            "listavailableequipment" -> return ListAvailableEquipment
            "getmotorizedstageposition" -> GetMotorizedStagePosition <$> v .: "name"
            "setmotorizedstageposition" -> SetMotorizedStagePosition <$> v .: "name" <*> v .: "position"
            "listavailabledetectors" -> return ListAvailableDetectors
            "getdetectorproperties" -> GetDetectorProperties <$> v .: "detectorname"
            "setdetectorproperty" -> SetDetectorProperty <$> v .: "detectorname" <*> v .: "property"
            "ping"      -> return Ping
            "executemeasurementprogram" -> ExecuteMeasurementProgram <$> v .: "program" <*> v .: "defineddetections" <*> v .: "smartprogramcode"
            "fetchasyncspectra" -> return FetchAsyncData
            "usesharedmemoryfortransfer" -> UseSharedMemoryForTransfer <$> v .: "usesharedmemory"
            "acknowledgedatareceipt" -> AcknowledgeDataReceipt <$> v .: "uptoandincluding"
            "fetchasyncstatusmessages" -> return FetchAsyncStatusMessages
            "cancelasyncacquisition" -> return CancelAsyncAcquisition
            "isasyncacquisitionrunning" -> return IsAsyncAcquisitionRunning
            _            -> fail $ "invalid action \"" ++ (T.unpack action) ++ "\""

    parseJSON _ = fail "expected a JSON object"

data ResponseMessage = StatusOK
                     | StatusError !String
                     | StatusNoNewAsyncData
                     | StatusNoNewAsyncDataComing
                     | StatusAcquiredDataCopiedToSharedMemory !Text
                     | AcquiredDataResponse ![ChannelMessage]
                     | Wavelengths !AcquiredData
                     | AvailableEquipment ![EquipmentW]
                     | MotorizedStagePosition !StagePosition
                     | AvailableDetectorsResponse ![DetectorName]
                     | DetectorPropertiesResponse ![DetectorProperty] Double
                     | Pong
                     | AsyncAcquiredData ![ChannelMessage]
                     | SharedMemoryNameResponse !Text
                     | AsyncStatusMessages ![Text]
                     | AsyncAcquisitionIsRunning !Bool
                     deriving (Generic)

instance ToJSON B.ByteString where
    toJSON = toJSON . T.decodeUtf8 . B64.encode  -- needed for the default-generated toJSON instances for ResponseMessages
    toEncoding = toEncoding . T.decodeUtf8 . B64.encode

instance ToJSON ResponseMessage where
    toEncoding StatusOK = pairs ("responsetype" .= ("status" :: Text) <> "status" .= ("ok" :: Text))
    toEncoding (StatusError s) = pairs ("responsetype" .= ("status" :: Text) <> "status" .= ("error"  :: Text) <> "error" .= s)
    toEncoding StatusNoNewAsyncData = pairs ("responsetype" .= ("asyncacquisitionspectrastatus" :: Text) <> "status" .= ("nonewspectra" :: Text))
    toEncoding (StatusNoNewAsyncDataComing) = pairs ("responsetype" .= ("asyncacquisitionspectrastatus" :: Text) <> "status" .= ("nonewspectracoming" :: Text))
    toEncoding (StatusAcquiredDataCopiedToSharedMemory shMemName) = pairs ("responsetype" .= ("acquireddatacopiedtosharedmemory" :: Text) <> "sharedmemoryname" .= shMemName)
    toEncoding (AcquiredDataResponse d) = pairs ("responsetype" .= ("acquireddata" :: Text) <> "data" .= d)
    toEncoding (Wavelengths d) = pairs ("responsetype" .= ("wavelengths" :: Text) <> "wavelengths" .= d)
    toEncoding (AvailableEquipment es) = pairs ("responsetype" .= ("availableequipment" :: Text) <> "equipment" .= es)
    toEncoding (MotorizedStagePosition ds) = pairs ("responsetype" .= ("motorizedstageposition" :: Text) <> "position" .= ds)
    toEncoding (AvailableDetectorsResponse ns) = pairs ("responsetype" .= ("availabledetectors" :: Text) <> "detectornames" .= ns)
    toEncoding (DetectorPropertiesResponse d fr) = pairs ("responsetype" .= ("detectorproperties" :: Text) <> "detectorproperties" .= d <>
                                                          "framerate" .= fr)
    toEncoding (Pong) = pairs ("responsetype" .= ("pong" :: Text))
    toEncoding (AsyncAcquiredData ds) =
        pairs ("responsetype" .= ("asyncdata" :: Text) <> "data" .= ds)
    toEncoding (SharedMemoryNameResponse name) = pairs ("responsetype" .= ("sharedmemoryname" :: Text) <> "name" .= name)
    toEncoding (AsyncStatusMessages ms) =
        pairs ("responsetype" .= ("asyncstatusmessages" :: Text) <> "messages" .= ms)
    toEncoding (AsyncAcquisitionIsRunning b) = pairs ("responsetype" .= ("asyncacquisitionstatus" :: Text) <> "running" .= b)

shouldBinaryEncode :: ResponseMessage -> Bool
shouldBinaryEncode (AcquiredDataResponse _) = True
shouldBinaryEncode (AsyncAcquiredData _) = True
shouldBinaryEncode (Wavelengths _) = True
shouldBinaryEncode _ = False

binaryEncode :: ResponseMessage -> [ByteString]
binaryEncode r@(AcquiredDataResponse ds) = encodeInMessagePack r
binaryEncode r@(AsyncAcquiredData ds) = encodeInMessagePack r
binaryEncode (Wavelengths d) = error "TODO: encoding wavelengths is unsupported for now" --encodeAcquiredData [(AcquisitionMetaData 0 (StagePosition (-1.0) (-1.0) (-1.0) False 0) "DUMMY", d)]
binaryEncode _ = error "no binary encoding for this type"

encodeInMessagePack :: ResponseMessage -> [ByteString]
encodeInMessagePack (AcquiredDataResponse ds) = map (B.toStrict . pack) ds
encodeInMessagePack (AsyncAcquiredData ds) = map (B.toStrict . pack) ds
encodeInMessagePack (Wavelengths d) = error "TODO: encoding wavelengths is unsupported for now" --encodeAcquiredData [(AcquisitionMetaData 0 (StagePosition (-1.0) (-1.0) (-1.0) False 0) "DUMMY", d)]
encodeInMessagePack _ = error "no binary encoding for this type"
