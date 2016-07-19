{-# LANGUAGE BangPatterns, OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module CuvettorTypes where

import Control.Concurrent
import Control.Concurrent.Async
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Unsafe as SB
import qualified Data.ByteString.Base64 as B64
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign
import System.IO.Unsafe

import MiscUtils
import GPIO
import OOSeaBreeze
import IrradiationProgram
import LightSources

data Environment = Environment {
                      envLightSources :: [LightSource]
                    , envGPIOHandles :: !GPIOHandles
                    , envAvailablePins :: [GPIOPin]
                    , envSpectrometer :: !(Maybe (DeviceID, FeatureID))
                    , envSpectrometerNonlinearityCorrection :: Double -> Double
                    , envEncodedSpectrometerWavelengths :: !Text
                    , envAsyncSpectraMVar :: MVar ([[(V.Vector Double, Double)]])
                    , envAsyncProgramWorker :: Async ()
}

type ExposureTime = Double

data RequestMessage = SetPinHigh !GPIOPin
                    | SetPinLow !GPIOPin
                    | AcquireSpectrum !DetectionParams
                    | SendWavelengths
                    | ListLightSources
                    | ActivateLightSource {
                        reqActivateName :: !Text
                      , reqActivateChannel :: !Text
                      , reqActivatePower :: !Double
                    }
                    | DeactivateLightSource !Text
                    | Ping
                    | ExecuteIrradiationProgram {
                        execIrradiationProgram :: !IrradiationProgram
                      }
                    | FetchAsyncSpectra
                    | CancelAsyncAcquisition
                    | IsAsyncAcquisitionRunning
                    deriving (Generic)

instance ToJSON RequestMessage where
    toEncoding (SetPinHigh pin) = pairs ("action" .= ("setpinhigh" :: Text) <> "pin" .= show pin)
    toEncoding (SetPinLow pin) = pairs ("action" .= ("setpinlow" :: Text) <> "pin" .= show pin)
    toEncoding (AcquireSpectrum p) = pairs ("action" .= ("acquirespectrum"  :: Text) <> "params" .= p)
    toEncoding SendWavelengths = pairs ("action" .= ("sendwavelengths"  :: Text))
    toEncoding ListLightSources = pairs ("action" .= ("listlightsources" :: Text))
    toEncoding (ActivateLightSource name channel power) = pairs ("action" .= ("activatelightsource" :: Text) <> "name" .= name <> "channel" .= channel <> "power" .= power)
    toEncoding (DeactivateLightSource name) = pairs ("action" .= ("deactivatelightsource" :: Text) <> "name" .= name)
    toEncoding Ping = pairs ("action" .= ("ping" :: Text))
    toEncoding (ExecuteIrradiationProgram prog) = pairs ("action" .= ("executeirradiationprogram" :: Text) <> "program" .= prog)
    toEncoding FetchAsyncSpectra = pairs ("action" .= ("fetchasyncspectra" :: Text))
    toEncoding CancelAsyncAcquisition = pairs ("action" .= ("cancelasyncacquisition" :: Text))
    toEncoding IsAsyncAcquisitionRunning = pairs ("action" .= ("isasyncacquisitionrunning" :: Text))

instance FromJSON RequestMessage where
    parseJSON (Object v) =
        v .: "action" >>= \action ->
        case (T.toLower action) of
            "setpinhigh" -> SetPinHigh <$> v .: "pin"
            "setpinlow"  -> SetPinLow <$> v .: "pin"
            "acquirespectrum" -> AcquireSpectrum <$> v .: "params"
            "sendwavelengths" -> return SendWavelengths
            "listlightsources" -> return ListLightSources
            "activatelightsource" -> ActivateLightSource <$> v .: "name" <*> v .: "channel" <*> v .: "power"
            "deactivatelightsource" -> DeactivateLightSource <$> v .: "name"
            "ping"      -> return Ping
            "executeirradiationprogram" -> ExecuteIrradiationProgram <$> v .: "program"
            "fetchasyncspectra" -> return FetchAsyncSpectra
            "cancelasyncacquisition" -> return CancelAsyncAcquisition
            "isasyncacquisitionrunning" -> return IsAsyncAcquisitionRunning
            _            -> fail $ "invalid action \"" ++ (T.unpack action) ++ "\""
    
    parseJSON _ = fail "expected a JSON object"

data ResponseMessage = StatusOK
                     | StatusError !String
                     | StatusNoNewAsyncSpectra
                     | StatusNoNewAsyncSpectraComing
                     | AcquiredSpectrum {
                         respAcqSpectrum   :: !SB.ByteString
                       , cachedWavelengths :: !Text
                       }
                     | Wavelengths !(Vector Double)
                     | AvailableLightSources ![LightSourceDesc]
                     | Pong
                     | AsyncAcquiredSpectra {
                         respAsyncSpectra :: ![[(SB.ByteString, Double)]]
                       , respAsyncCachedWavelengths :: !Text
                       }
                     | AsyncAcquisitionIsRunning !Bool
                     deriving (Generic)

instance ToJSON SB.ByteString where
    toJSON = toJSON . T.decodeUtf8  -- needed for the default-generated toJSON instances for ResponseMessages

instance ToJSON ResponseMessage where
    toEncoding StatusOK = pairs ("responsetype" .= ("status" :: Text) <> "status" .= ("ok" :: Text))
    toEncoding (StatusError s) = pairs ("responsetype" .= ("status" :: Text) <> "status" .= ("error"  :: Text) <> "error" .= s)
    toEncoding StatusNoNewAsyncSpectra = pairs ("responsetype" .= ("asyncacquisitionspectrastatus" :: Text) <> "status" .= ("nonewspectra" :: Text))
    toEncoding (StatusNoNewAsyncSpectraComing) = pairs ("responsetype" .= ("asyncacquisitionspectrastatus" :: Text) <> "status" .= ("nonewspectracoming" :: Text))
    toEncoding (AcquiredSpectrum v w) = pairs ("responsetype" .= ("spectrum" :: Text) <> "spectrum" .= (T.decodeUtf8 . B64.encode $ v)
                                                <> "wavelengths" .= w)
    toEncoding (Wavelengths v) = pairs ("responsetype" .= ("wavelengths" :: Text) <> "wavelengths" .= (T.decodeUtf8 . B64.encode . byteStringFromVector $ v))
    toEncoding (AvailableLightSources ls) = pairs ("responsetype" .= ("availablelightsources" :: Text) <> "lightsources" .= ls)
    toEncoding (Pong) = pairs ("responsetype" .= ("pong" :: Text))
    toEncoding (AsyncAcquiredSpectra spectra w) = 
        let encodedByteStrings = map (map (\(v, t) -> (T.decodeUtf8 . B64.encode $ v, t))) spectra
        in pairs ("responsetype" .= ("asyncspectra" :: Text) <> "spectra" .= encodedByteStrings <> "wavelengths" .= w)
    toEncoding (AsyncAcquisitionIsRunning b) = pairs ("responsetype" .= ("asyncacquisitionstatus" :: Text) <> "running" .= b)

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
    toJSON Pin2 = "pin2"
    toJSON Pin3 = "pin3"
    toJSON Pin4 = "pin4"
    toJSON Pin7 = "pin7"
    toJSON Pin8 = "pin8"
    toJSON Pin9 = "pin9"
    toJSON Pin10 = "pin10"
    toJSON Pin11 = "pin11"
    toJSON Pin17 = "pin17"
