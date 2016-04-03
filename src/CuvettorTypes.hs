{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module CuvettorTypes where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import GPIO

data Environment = Environment {
                      envGPIOHandles :: !GPIOHandles
                    , envAvailablePins :: [GPIOPin]
}

type ExposureTime = Double

data RequestMessage = SetPinHigh !GPIOPin
                    | SetPinLow !GPIOPin
                    | AcquireSpectrum {
                        exposureTime :: !ExposureTime
                      , nSpectra :: !Int
                    }
                    | SendWavelengths

instance ToJSON RequestMessage where
    toJSON (SetPinHigh pin) = object [("action", "setpinhigh"), "pin" .= show pin]
    toJSON (SetPinLow pin) = object [("action", "setpinlow"), "pin" .= show pin]
    toJSON (AcquireSpectrum e n) = object [("action", "acquirespectrum"), "exposureTime" .= show e, "nspectra" .= show n]
    toJSON SendWavelengths = object [("action", "sendwavelengths")]

instance FromJSON RequestMessage where
    parseJSON (Object v) =
        v .: "action" >>= \action ->
        case (T.toLower action) of
            "setpinhigh" -> SetPinHigh <$> v .: "pin"
            "setpinlow"  -> SetPinLow <$> v .: "pin"
            "acquirespectrum" -> AcquireSpectrum <$> v .: "exposureTime" <*> v .: "nspectra"
            "sendwavelengths" -> return SendWavelengths
            _                 -> fail $ "invalid action \"" ++ (T.unpack action) ++ "\""
    
    parseJSON _ = fail "expected a JSON object"

data ResponseMessage = StatusOK
                     | StatusError
                     | AcquiredSpectrum !(Vector Double)
                     | Wavelengths !(Vector Double)

instance ToJSON ResponseMessage where
    toJSON StatusOK = object [("responsetype", "status"), ("status", "ok")]
    toJSON StatusError = object [("responsetype", "status"), ("status", "error")]
    toJSON (AcquiredSpectrum v) = object [("responsetype", "spectrum"), "spectrum" .= (V.toList v)]
    toJSON (Wavelengths v) = object [("responsetype", "wavelengths"), "wavelengths" .= (V.toList v)]

instance FromJSON GPIOPin where
    parseJSON (String s) =
        case (T.toLower s) of
            "pin2"  -> return Pin2
            "pin3"  -> return Pin3
            "pin4"  -> return Pin4
            "pin7"  -> return Pin7
            "pin8"  -> return Pin8
            "pin9"  -> return Pin9
            "pin10"  -> return Pin10
            "pin11"  -> return Pin11
    parseJSON invalid = fail "can't decode gpio pin"
