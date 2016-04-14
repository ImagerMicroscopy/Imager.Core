{-# LANGUAGE BangPatterns, OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module CuvettorTypes where

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

import GPIO
import OOSeaBreeze

data Environment = Environment {
                      envGPIOHandles :: !GPIOHandles
                    , envAvailablePins :: [GPIOPin]
                    , envSpectrometer :: !(Maybe (DeviceID, FeatureID))
}

type ExposureTime = Double

data RequestMessage = SetPinHigh !GPIOPin
                    | SetPinLow !GPIOPin
                    | AcquireSpectrum {
                        exposureTime :: !ExposureTime
                      , nSpectra :: !Int
                      }
                    | SendWavelengths
                    | Ping
                    deriving (Generic)

instance ToJSON RequestMessage where
    toEncoding (SetPinHigh pin) = pairs ("action" .= ("setpinhigh" :: Text) <> "pin" .= show pin)
    toEncoding (SetPinLow pin) = pairs ("action" .= ("setpinlow" :: Text) <> "pin" .= show pin)
    toEncoding (AcquireSpectrum e n) = pairs ("action" .= ("acquirespectrum"  :: Text) <> "exposuretime" .= e <> "nspectra" .= n)
    toEncoding SendWavelengths = pairs ("action" .= ("sendwavelengths"  :: Text))
    toEncoding Ping = pairs ("action" .= ("ping" :: Text))

instance FromJSON RequestMessage where
    parseJSON (Object v) =
        v .: "action" >>= \action ->
        case (T.toLower action) of
            "setpinhigh" -> SetPinHigh <$> v .: "pin"
            "setpinlow"  -> SetPinLow <$> v .: "pin"
            "acquirespectrum" -> AcquireSpectrum <$> v .: "exposuretime" <*> v .: "nspectra"
            "sendwavelengths" -> return SendWavelengths
            "ping"      -> return Ping
            _                 -> fail $ "invalid action \"" ++ (T.unpack action) ++ "\""
    
    parseJSON _ = fail "expected a JSON object"

data ResponseMessage = StatusOK
                     | StatusError !String
                     | AcquiredSpectrum !(Vector Double)
                     | Wavelengths !(Vector Double)
                     | Pong
                     deriving (Generic)

instance ToJSON ResponseMessage where
    toEncoding StatusOK = pairs ("responsetype" .= ("status" :: Text) <> "status" .= ("ok" :: Text))
    toEncoding (StatusError s) = pairs ("responsetype" .= ("status" :: Text) <> "status" .= ("error"  :: Text) <> "error" .= s)
    toEncoding (AcquiredSpectrum v) = pairs ("responsetype" .= ("spectrum" :: Text) <> "spectrum" .= (T.decodeUtf8 . B64.encode $ byteStringFromVector v))
    toEncoding (Wavelengths v) = pairs ("responsetype" .= ("wavelengths" :: Text) <> "wavelengths" .= (T.decodeUtf8 . B64.encode $ byteStringFromVector v))
    toEncoding (Pong) = pairs ("responsetype" .= ("pong" :: Text))

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

byteStringFromVector :: forall a . Storable a => Vector a -> SB.ByteString
byteStringFromVector v = unsafePerformIO $
    let sizeOfElem = sizeOf (undefined :: a)
        nElems = V.length v
        nBytes = nElems * sizeOfElem in
    V.unsafeWith v $ \vecPtr ->
    mallocBytes nBytes >>= \bsPtr ->
    copyBytes (castPtr bsPtr) vecPtr nBytes >>
    SB.unsafePackMallocCStringLen (bsPtr, nBytes)
