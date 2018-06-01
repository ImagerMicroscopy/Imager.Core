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

import AcquiredDataTypes
import MiscUtils
import Detector
import Equipment
import EquipmentEncoding
import EquipmentTypes
import MeasurementProgram
import MeasurementProgramTypes

data Environment a = Environment {
                      envEquipment :: ![EquipmentW]
                    , envDetector :: !a
                    , envEncodedSpectrometerWavelengths :: !SB.ByteString
                    , envAsyncDataMVar :: !(MVar [(AcquisitionMetaData, AcquiredData)])
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
                    | GetDetectorLimits !(Int, Int) Int
                    | GetDetectorParameters
                    | SetDetectorTemperature !Double
                    | GetDetectorTemperature
                    | ActivateLightSource {
                        alsEquipmentName :: !EqName
                      , alsName :: !LSName
                      , alsChannels :: ![LSChannelName]
                      , alsPowers :: ![LSIlluminationPower]
                    }
                    | DeactivateLightSource !EqName
                    | TurnOffLightSource !EqName
                    | Ping
                    | ExecuteMeasurementProgram {
                        execMeasurementProgram :: !MeasurementElement
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
    toEncoding (GetDetectorLimits cropSize binFactor) = pairs ("action" .= ("getdetectorlimits" :: Text) <> "binfactor" .= binFactor <>
                                                               "nrows" .= fst cropSize <> "ncols" .= snd cropSize)
    toEncoding GetDetectorParameters = pairs ("action" .= ("getdetectorparameters" :: Text))
    toEncoding (SetDetectorTemperature t) = pairs ("action" .= ("setdetectortemperature" :: Text) <> "temperature" .= t)
    toEncoding GetDetectorTemperature = pairs ("action" .= ("getdetectortemperature" :: Text))
    toEncoding (ActivateLightSource eqName name channel power) =
        pairs ("action" .= ("activatelightsource" :: Text) <> "equipmentname" .= eqName <>
               "name" .= name <> "channel" .= channel <> "power" .= power)
    toEncoding (DeactivateLightSource name) = pairs ("action" .= ("deactivatelightsource" :: Text) <> "name" .= name)
    toEncoding (TurnOffLightSource name) = pairs ("action" .= ("turnofflightsource" :: Text) <> "name" .= name)
    toEncoding Ping = pairs ("action" .= ("ping" :: Text))
    toEncoding (ExecuteMeasurementProgram prog) = pairs ("action" .= ("executemeasurementprogram" :: Text) <> "program" .= prog)
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
            "getdetectorlimits" -> v .: "nrows" >>= \nRows -> v .: "ncols" >>= \nCols ->
                                   GetDetectorLimits (nRows, nCols) <$> v .: "binfactor"
            "getdetectorparameters" -> return GetDetectorParameters
            "setdetectortemperature" -> SetDetectorTemperature <$> v .: "temperature"
            "getdetectortemperature" -> return GetDetectorTemperature
            "activatelightsource" -> ActivateLightSource <$> v .: "equipmentname"
                                                         <*> v .: "name"
                                                         <*> v .: "channel"
                                                         <*> v .: "power"
            "deactivatelightsource" -> DeactivateLightSource <$> v .: "name"
            "turnofflightsource" -> TurnOffLightSource <$> v .: "name"
            "ping"      -> return Ping
            "executemeasurementprogram" -> ExecuteMeasurementProgram <$> v .: "program"
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
                     | AcquiredDataResponse !(AcquisitionMetaData, AcquiredData)
                     | Wavelengths !AcquiredData
                     | AvailableEquipment ![EquipmentW]
                     | MotorizedStagePosition !StagePosition
                     | RobotProgramsResponse ![RobotProgramName]
                     | DetectorLimitsResponse !DetectorLimits
                     | DetectorParametersResponse !DetectorParameters
                     | DetectorTemperatureResponse !Double
                     | Pong
                     | AsyncAcquiredData ![(AcquisitionMetaData, AcquiredData)]
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
    toEncoding (DetectorLimitsResponse dl) = pairs ("responsetype" .= ("detectorlimits" :: Text) <> "detectorlimits" .= dl)
    toEncoding (DetectorParametersResponse d) = pairs ("responsetype" .= ("detectorparameters" :: Text) <> "parameters" .= d)
    toEncoding (DetectorTemperatureResponse t) = pairs ("responsetype" .= ("detectortemperature" :: Text) <> "detectortemperature" .= t)
    toEncoding (Pong) = pairs ("responsetype" .= ("pong" :: Text))
    toEncoding (AsyncAcquiredData ds) =
        pairs ("responsetype" .= ("asyncdata" :: Text) <> "data" .= ds)
    toEncoding (AsyncStatusMessages ms) =
        pairs ("responsetype" .= ("asyncstatusmessages" :: Text) <> "messages" .= ms)
    toEncoding (AsyncAcquisitionIsRunning b) = pairs ("responsetype" .= ("asyncacquisitionstatus" :: Text) <> "running" .= b)

instance ToJSON DetectorParameters where
    toJSON (DetectorParameters dataSize allowedCrop allowedBinning currentBin limits tempSetPoint) =
        object [("datadimensions", object ["nrows" .= fst dataSize, "ncols" .= snd dataSize]),
                ("allowedcropsizes", toJSON (map (\(r,c) -> object ["nrows" .= r, "ncols" .= c]) allowedCrop)),
                "allowedbinfactors" .= allowedBinning, "currentbinfactor" .= currentBin,
                "limits" .= limits, "temperaturesetpoint" .= tempSetPoint]

instance ToJSON DetectorLimits where
    toJSON (DetectorLimits minExpTime maxExpTime minGain maxGain minAveraging maxAveraging) =
        object ["minexposuretime" .= minExpTime, "maxexposuretime" .= maxExpTime,
                "mingain" .= minGain, "maxgain" .= maxGain,
                "minaveraging" .= minAveraging, "maxaveraging" .= maxAveraging]
