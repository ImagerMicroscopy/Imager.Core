{-# LANGUAGE BangPatterns, OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module CuvettorTypes where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Parallel.Strategies
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Unsafe as SB
import qualified Data.ByteString.Base64 as B64
import Data.Char
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign
import System.Clock
import System.IO.Unsafe

import Utils.MiscUtils
import Detectors.Detector
import Equipment.Equipment
import Encodings.EquipmentEncoding
import Equipment.EquipmentTypes
import Measurements.MeasurementProgram
import Measurements.MeasurementProgramTypes

data Environment a = Environment {
                      envEquipment :: ![EquipmentW]
                    , envDetectors :: ![a]
                    , envEncodedSpectrometerWavelengths :: !SB.ByteString
                    , envAsyncDataChannel :: !MessageChannel
                    , envAsyncStatusMessagesMVar :: !(MVar [Text])
                    , envAsyncProgramWorker :: !(Async ())
}

type ExposureTime = Double

data RequestMessage = AcquireData !DetectionParams
                    | ListWavelengths
                    | ListAvailableEquipment
                    | GetMotorizedStagePosition !StageName
                    | SetMotorizedStagePosition !StageName !StagePosition
                    | ListRobotPrograms !RobotName
                    | ListAvailableDetectors
                    | GetDetectorProperties !Text
                    | SetDetectorProperty !Text !DetectorProperty
                    | Ping
                    | ExecuteMeasurementProgram {
                          execMeasurementProgram :: !MeasurementElement
                        , execMeasurementDetections :: !DefinedDetections
                        , execMeasurementSmartProgramCode :: !(Maybe SmartProgramCode)
                      }
                    | FetchAsyncData
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
    toEncoding (ListRobotPrograms name) = pairs ("action" .= ("listrobotprograms" :: Text) <> "name" .= name)
    toEncoding ListAvailableDetectors = pairs ("action" .= ("listavailabledetectors" :: Text))
    toEncoding (GetDetectorProperties detName) = pairs ("action" .= ("getdetectorproperties" :: Text) <> "detectorname" .= detName)
    toEncoding (SetDetectorProperty detName prop) = pairs ("action" .= ("setdetectorproperty" :: Text) <> "detectorname" .= detName <> "property" .= prop)
    toEncoding Ping = pairs ("action" .= ("ping" :: Text))
    toEncoding (ExecuteMeasurementProgram prog dets smartprog) = pairs (
        "action" .= ("executemeasurementprogram" :: Text) <>
        "program" .= prog <>
        "defineddetections" .= dets <>
        "smartprogramcode" .= (fromSmartProgramCode <$> smartprog))
    toEncoding FetchAsyncData = pairs ("action" .= ("fetchasyncspectra" :: Text))
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
            "listrobotprograms" -> ListRobotPrograms <$> v .: "name"
            "listavailabledetectors" -> return ListAvailableDetectors
            "getdetectorproperties" -> GetDetectorProperties <$> v .: "detectorname"
            "setdetectorproperty" -> SetDetectorProperty <$> v .: "detectorname" <*> v .: "property"
            "ping"      -> return Ping
            "executemeasurementprogram" -> ExecuteMeasurementProgram <$> v .: "program" <*> v .: "defineddetections" <*> v .:? "smartprogramcode"
            "fetchasyncspectra" -> return FetchAsyncData
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
                     | AcquiredDataResponse ![ChannelMessage]
                     | Wavelengths !AcquiredData
                     | AvailableEquipment ![EquipmentW]
                     | MotorizedStagePosition !StagePosition
                     | RobotProgramsResponse ![RobotProgramName]
                     | AvailableDetectorsResponse ![Text]
                     | DetectorPropertiesResponse ![DetectorProperty] Double
                     | Pong
                     | AsyncAcquiredData ![ChannelMessage]
                     | AsyncStatusMessages ![Text]
                     | AsyncAcquisitionIsRunning !Bool
                     deriving (Generic)

instance ToJSON SB.ByteString where
    toJSON = toJSON . T.decodeUtf8 . B64.encode  -- needed for the default-generated toJSON instances for ResponseMessages
    toEncoding = toEncoding . T.decodeUtf8 . B64.encode

instance ToJSON ResponseMessage where
    toEncoding StatusOK = pairs ("responsetype" .= ("status" :: Text) <> "status" .= ("ok" :: Text))
    toEncoding (StatusError s) = pairs ("responsetype" .= ("status" :: Text) <> "status" .= ("error"  :: Text) <> "error" .= s)
    toEncoding StatusNoNewAsyncData = pairs ("responsetype" .= ("asyncacquisitionspectrastatus" :: Text) <> "status" .= ("nonewspectra" :: Text))
    toEncoding (StatusNoNewAsyncDataComing) = pairs ("responsetype" .= ("asyncacquisitionspectrastatus" :: Text) <> "status" .= ("nonewspectracoming" :: Text))
    toEncoding (AcquiredDataResponse d) = pairs ("responsetype" .= ("acquireddata" :: Text) <> "data" .= d)
    toEncoding (Wavelengths d) = pairs ("responsetype" .= ("wavelengths" :: Text) <> "wavelengths" .= d)
    toEncoding (AvailableEquipment es) = pairs ("responsetype" .= ("availableequipment" :: Text) <> "equipment" .= es)
    toEncoding (MotorizedStagePosition ds) = pairs ("responsetype" .= ("motorizedstageposition" :: Text) <> "position" .= ds)
    toEncoding (RobotProgramsResponse ps) = pairs ("responsetype" .= ("robotprograms" :: Text) <> "programs" .= ps)
    toEncoding (AvailableDetectorsResponse ns) = pairs ("responsetype" .= ("availabledetectors" :: Text) <> "detectornames" .= ns)
    toEncoding (DetectorPropertiesResponse d fr) = pairs ("responsetype" .= ("detectorproperties" :: Text) <> "detectorproperties" .= d <>
                                                          "framerate" .= fr)
    toEncoding (Pong) = pairs ("responsetype" .= ("pong" :: Text))
    toEncoding (AsyncAcquiredData ds) =
        pairs ("responsetype" .= ("asyncdata" :: Text) <> "data" .= ds)
    toEncoding (AsyncStatusMessages ms) =
        pairs ("responsetype" .= ("asyncstatusmessages" :: Text) <> "messages" .= ms)
    toEncoding (AsyncAcquisitionIsRunning b) = pairs ("responsetype" .= ("asyncacquisitionstatus" :: Text) <> "running" .= b)
