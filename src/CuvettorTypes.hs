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
import Foreign
import System.Clock
import System.IO.Unsafe

import CameraImageProcessing
import MiscUtils
import Detector
import EquipmentEncoding
import EquipmentTypes
import MeasurementProgram
import MeasurementProgramTypes
import LightSources
import FilterWheel
import MotorizedStage
import Robot

data Environment a = Environment {
                      envLightSources :: [Equipment]
                    , envFilterWheels :: [Equipment]
                    , envMotorizedStages :: [Equipment]
                    , envRobots :: [Equipment]
                    , envDetector :: a
                    , envRearrangementFuncs :: [ExternalRearrangementFunc]
                    , envEncodedSpectrometerWavelengths :: !SB.ByteString
                    , envAsyncDataMVar :: MVar [AcquiredData]
                    , envAsyncStatusMessagesMVar :: MVar [Text]
                    , envAsyncProgramWorker :: Async ()
}

type ExposureTime = Double

data RequestMessage = AcquireData !DetectionParams
                    | ListWavelengths
                    | ListLightSources
                    | ListFilterWheels
                    | ListMotorizedStages
                    | ListRobots
                    | GetMotorizedStagePosition !Text
                    | SetMotorizedStagePosition !Text !(Double, Double, Double)
                    | ListRobotPrograms !Text
                    | GetDetectorLimits
                    | SetDetectorTemperature !Double
                    | GetDetectorTemperature
                    | GetDetectorTemperatureSetpoint
                    | ActivateLightSource {
                        reqActivateName :: !Text
                      , reqActivateChannels :: ![Text]
                      , reqActivatePowers :: ![Double]
                    }
                    | DeactivateLightSource !Text
                    | TurnOffLightSource !Text
                    | Ping
                    | ExecuteMeasurementProgram {
                        execMeasurementProgram :: !MeasurementElement
                      }
                    | FetchAsyncData
                    | FetchAsyncStatusMessages
                    | CancelAsyncAcquisition
                    | IsAsyncAcquisitionRunning
                    deriving (Generic)

instance ToJSON RequestMessage where
    toEncoding ListWavelengths = pairs ("action" .= ("listwavelengths" :: Text))
    toEncoding (AcquireData p) = pairs ("action" .= ("acquiredata"  :: Text) <> "params" .= p)
    toEncoding ListLightSources = pairs ("action" .= ("listlightsources" :: Text))
    toEncoding ListFilterWheels = pairs ("action" .= ("listfilterwheels" :: Text))
    toEncoding ListMotorizedStages = pairs ("action" .= ("listmotorizedstages" :: Text))
    toEncoding ListRobots = pairs ("action" .= ("listmicroscoperobots" :: Text))
    toEncoding (GetMotorizedStagePosition name) = pairs ("action" .= ("getmotorizedstageposition" :: Text) <> "name" .= name)
    toEncoding (SetMotorizedStagePosition name ds) = pairs ("action" .= ("setmotorizedstageposition" :: Text) <> "name" .= name <> "position" .= ds)
    toEncoding (ListRobotPrograms name) = pairs ("action" .= ("listrobotprograms" :: Text) <> "name" .= name)
    toEncoding GetDetectorLimits = pairs ("action" .= ("getdetectorlimits" :: Text))
    toEncoding (SetDetectorTemperature t) = pairs ("action" .= ("setdetectortemperature" :: Text) <> "temperature" .= t)
    toEncoding GetDetectorTemperature = pairs ("action" .= ("getdetectortemperature" :: Text))
    toEncoding GetDetectorTemperatureSetpoint = pairs ("action" .= ("getdetectortemperaturesetpoint" :: Text))
    toEncoding (ActivateLightSource name channel power) = pairs ("action" .= ("activatelightsource" :: Text) <> "name" .= name <> "channel" .= channel <> "power" .= power)
    toEncoding (DeactivateLightSource name) = pairs ("action" .= ("deactivatelightsource" :: Text) <> "name" .= name)
    toEncoding (TurnOffLightSource name) = pairs ("action" .= ("turnofflightsource" :: Text) <> "name" .= name)
    toEncoding Ping = pairs ("action" .= ("ping" :: Text))
    toEncoding (ExecuteMeasurementProgram prog) = pairs ("action" .= ("executemeasurementprogram" :: Text) <> "program" .= prog)
    toEncoding FetchAsyncData = pairs ("action" .= ("fetchasyncspectra" :: Text))
    toEncoding FetchAsyncStatusMessages = pairs ("action" .= ("fetchasyncstatusmessages" :: Text))
    toEncoding CancelAsyncAcquisition = pairs ("action" .= ("cancelasyncacquisition" :: Text))
    toEncoding IsAsyncAcquisitionRunning = pairs ("action" .= ("isasyncacquisitionrunning" :: Text))

instance FromJSON RequestMessage where
    parseJSON (Object v) =
        v .: "action" >>= \action ->
        case (T.toLower action) of
            "acquiredata" -> AcquireData <$> v .: "params"
            "listwavelengths" -> return ListWavelengths
            "listlightsources" -> return ListLightSources
            "listfilterwheels" -> return ListFilterWheels
            "listmotorizedstages" -> return ListMotorizedStages
            "listrobots" -> return ListRobots
            "getmotorizedstageposition" -> GetMotorizedStagePosition <$> v .: "name"
            "setmotorizedstageposition" -> SetMotorizedStagePosition <$> v .: "name" <*> v .: "position"
            "listrobotprograms" -> ListRobotPrograms <$> v .: "name"
            "getdetectorlimits" -> return GetDetectorLimits
            "setdetectortemperature" -> SetDetectorTemperature <$> v .: "temperature"
            "getdetectortemperature" -> return GetDetectorTemperature
            "getdetectortemperaturesetpoint" -> return GetDetectorTemperatureSetpoint
            "activatelightsource" -> ActivateLightSource <$> v .: "name" <*> v .: "channel" <*> v .: "power"
            "deactivatelightsource" -> DeactivateLightSource <$> v .: "name"
            "turnofflightsource" -> TurnOffLightSource <$> v .: "name"
            "ping"      -> return Ping
            "executemeasurementprogram" -> ExecuteMeasurementProgram <$> v .: "program"
            "fetchasyncspectra" -> return FetchAsyncData
            "fetchasyncstatusmessages" -> return FetchAsyncStatusMessages
            "cancelasyncacquisition" -> return CancelAsyncAcquisition
            "isasyncacquisitionrunning" -> return IsAsyncAcquisitionRunning
            _            -> fail $ "invalid action \"" ++ (T.unpack action) ++ "\""

    parseJSON _ = fail "expected a JSON object"

data ResponseMessage = StatusOK
                     | StatusError !String
                     | StatusNoNewAsyncData
                     | StatusNoNewAsyncDataComing
                     | AcquiredDataResponse !AcquiredData
                     | Wavelengths !AcquiredData
                     | AvailableLightSources ![Equipment]
                     | AvailableFilterWheels ![Equipment]
                     | AvailableMotorizedStages ![Equipment]
                     | AvailableRobots ![Equipment]
                     | MotorizedStagePosition !(Double, Double, Double)
                     | RobotProgramsResponse ![Text]
                     | DetectorLimitsResponse !DetectorLimits
                     | DetectorTemperatureResponse !Double
                     | DetectorTemperatureSetpointResponse !Double
                     | Pong
                     | AsyncAcquiredData ![AcquiredData]
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
    toEncoding (AvailableLightSources ls) = pairs ("responsetype" .= ("availablelightsources" :: Text) <> "lightsources" .= ls)
    toEncoding (AvailableFilterWheels fws) = pairs ("responsetype" .= ("availablefilterwheels" :: Text) <> "filterwheels" .= fws)
    toEncoding (AvailableMotorizedStages ss) = pairs ("responsetype" .= ("availablemotorizedstages" :: Text) <> "motorizedstages" .= ss)
    toEncoding (AvailableRobots ss) = pairs ("responsetype" .= ("availablerobots" :: Text) <> "robots" .= ss)
    toEncoding (MotorizedStagePosition ds) = pairs ("responsetype" .= ("motorizedstageposition" :: Text) <> "position" .= ds)
    toEncoding (RobotProgramsResponse ps) = pairs ("responsetype" .= ("robotprograms" :: Text) <> "programs" .= ps)
    toEncoding (DetectorLimitsResponse dl) = pairs ("responsetype" .= ("detectorlimits" :: Text) <> "detectorlimits" .= dl)
    toEncoding (DetectorTemperatureResponse t) = pairs ("responsetype" .= ("detectortemperature" :: Text) <> "detectortemperature" .= t)
    toEncoding (DetectorTemperatureSetpointResponse t) = pairs ("responsetype" .= ("detectortemperaturesetpoint" :: Text) <> "detectortemperaturesetpoint" .= t)
    toEncoding (Pong) = pairs ("responsetype" .= ("pong" :: Text))
    toEncoding (AsyncAcquiredData ds) =
        pairs ("responsetype" .= ("asyncdata" :: Text) <> "data" .= ds)
    toEncoding (AsyncStatusMessages ms) =
        pairs ("responsetype" .= ("asyncstatusmessages" :: Text) <> "messages" .= ms)
    toEncoding (AsyncAcquisitionIsRunning b) = pairs ("responsetype" .= ("asyncacquisitionstatus" :: Text) <> "running" .= b)

instance ToJSON AcquiredData where
  toJSON (AcquiredData nRows nCols timeStamp bytes numType) =
      object ["nrows" .= nRows, "ncols" .= nCols, "data" .= bytes, "timestamp" .= (timeSpecAsDouble timeStamp), "numtype" .= (show numType)]
  toEncoding (AcquiredData nRows nCols timeStamp bytes numType) =
      pairs ("nrows" .= nRows <> "ncols" .= nCols <> "timestamp" .= (timeSpecAsDouble timeStamp) <> "data" .= bytes <> "numtype" .= (show numType))

instance ToJSON DetectorLimits where
    toJSON (DetectorLimits minExpTime maxExpTime minGain maxGain minAveraging maxAveraging) =
        object ["minexposuretime" .= minExpTime, "maxexposuretime" .= maxExpTime,
                "mingain" .= minGain, "maxgain" .= maxGain,
                "minaveraging" .= minAveraging, "maxaveraging" .= maxAveraging]
