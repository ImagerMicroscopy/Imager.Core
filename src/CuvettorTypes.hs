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

import MiscUtils
import Detector
import GPIO
import IrradiationProgram
import LightSources

data Environment a = Environment {
                      envLightSourceDescs :: [LightSourceDesc]
                    , envLightSources :: [LightSource]
                    , envGPIOHandles :: !GPIOHandles
                    , envAvailablePins :: [GPIOPin]
                    , envDetector :: a
                    , envEncodedSpectrometerWavelengths :: !SB.ByteString
                    , envAsyncDataMVar :: MVar [[AcquiredData]]
                    , envAsyncProgramWorker :: Async ()
}

type ExposureTime = Double

data RequestMessage = SetPinHigh !GPIOPin
                    | SetPinLow !GPIOPin
                    | AcquireData !DetectionParams
                    | ListWavelengths
                    | ListLightSources
                    | ActivateLightSource {
                        reqActivateName :: !Text
                      , reqActivateChannels :: ![Text]
                      , reqActivatePowers :: ![Double]
                    }
                    | DeactivateLightSource !Text
                    | Ping
                    | ExecuteIrradiationProgram {
                        execIrradiationProgram :: !IrradiationProgram
                      }
                    | FetchAsyncData
                    | CancelAsyncAcquisition
                    | IsAsyncAcquisitionRunning
                    deriving (Generic)

instance ToJSON RequestMessage where
    toEncoding (SetPinHigh pin) = pairs ("action" .= ("setpinhigh" :: Text) <> "pin" .= show pin)
    toEncoding (SetPinLow pin) = pairs ("action" .= ("setpinlow" :: Text) <> "pin" .= show pin)
    toEncoding ListWavelengths = pairs ("action" .= ("listwavelengths" :: Text))
    toEncoding (AcquireData p) = pairs ("action" .= ("acquiredata"  :: Text) <> "params" .= p)
    toEncoding ListLightSources = pairs ("action" .= ("listlightsources" :: Text))
    toEncoding (ActivateLightSource name channel power) = pairs ("action" .= ("activatelightsource" :: Text) <> "name" .= name <> "channel" .= channel <> "power" .= power)
    toEncoding (DeactivateLightSource name) = pairs ("action" .= ("deactivatelightsource" :: Text) <> "name" .= name)
    toEncoding Ping = pairs ("action" .= ("ping" :: Text))
    toEncoding (ExecuteIrradiationProgram prog) = pairs ("action" .= ("executeirradiationprogram" :: Text) <> "program" .= prog)
    toEncoding FetchAsyncData = pairs ("action" .= ("fetchasyncspectra" :: Text))
    toEncoding CancelAsyncAcquisition = pairs ("action" .= ("cancelasyncacquisition" :: Text))
    toEncoding IsAsyncAcquisitionRunning = pairs ("action" .= ("isasyncacquisitionrunning" :: Text))

instance FromJSON RequestMessage where
    parseJSON (Object v) =
        v .: "action" >>= \action ->
        case (T.toLower action) of
            "setpinhigh" -> SetPinHigh <$> v .: "pin"
            "setpinlow"  -> SetPinLow <$> v .: "pin"
            "acquiredata" -> AcquireData <$> v .: "params"
            "listwavelengths" -> return ListWavelengths
            "listlightsources" -> return ListLightSources
            "activatelightsource" -> ActivateLightSource <$> v .: "name" <*> v .: "channel" <*> v .: "power"
            "deactivatelightsource" -> DeactivateLightSource <$> v .: "name"
            "ping"      -> return Ping
            "executeirradiationprogram" -> ExecuteIrradiationProgram <$> v .: "program"
            "fetchasyncspectra" -> return FetchAsyncData
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
                     | AvailableLightSources ![LightSourceDesc]
                     | Pong
                     | AsyncAcquiredData ![[AcquiredData]]
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
    toEncoding (Pong) = pairs ("responsetype" .= ("pong" :: Text))
    toEncoding (AsyncAcquiredData ds) =
        pairs ("responsetype" .= ("asyncdata" :: Text) <> "data" .= ds)
    toEncoding (AsyncAcquisitionIsRunning b) = pairs ("responsetype" .= ("asyncacquisitionstatus" :: Text) <> "running" .= b)

instance ToJSON AcquiredData where
  toJSON (AcquiredData nRows nCols timeStamp bytes numType) =
      object ["nrows" .= nRows, "ncols" .= nCols, "data" .= bytes, "timestamp" .= (timeSpecAsDouble timeStamp), "numtype" .= (show numType)]
  toEncoding (AcquiredData nRows nCols timeStamp bytes numType) =
      pairs ("nrows" .= nRows <> "ncols" .= nCols <> "timestamp" .= (timeSpecAsDouble timeStamp) <> "data" .= bytes <> "numtype" .= (show numType))

instance FromJSON GPIOPin where
    parseJSON (String s) =
        case (T.toLower s) of
            "pin2"  -> return Pin2
            "pin3"  -> return Pin3
            "pin4"  -> return Pin4
            "pin7"  -> return Pin7
            "pin8"  -> return Pin8
            "pin9"  -> return Pin9
            "pin10" -> return Pin10
            "pin11" -> return Pin11
            "pin17" -> return Pin17
    parseJSON invalid = fail "can't decode gpio pin"

instance ToJSON GPIOPin where
    toJSON = toJSON . map toLower . show
    toEncoding = toEncoding . map toLower . show
