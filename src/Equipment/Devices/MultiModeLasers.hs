{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Equipment.Devices.MultiModeLasers where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char
import Data.Either
import Data.Set (Set)
import qualified Data.Set as S
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word

import Equipment.Equipment
import Equipment.EquipmentTypes
import Utils.MiscUtils
import RCSerialPort

data MultiModeLasers = MultiModeLasers !EqName !SerialPort

data MMLaser = MMRed | MMGreen

initializeMultiModeLasersLightSource :: EquipmentDescription -> IO EquipmentW
initializeMultiModeLasersLightSource (MultiModeLasersDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 1000) SerialPortDebugText
    in  openSerialPort portName serialSettings >>= \port -> threadDelay (floor 2e6) >> -- delay needed, otherwise the arduino won't receive the messages.
        return (EquipmentW (MultiModeLasers (EqName name) port))

instance Equipment MultiModeLasers where
    equipmentName (MultiModeLasers n _) = n
    flushSerialPorts (MultiModeLasers _ p) = flushSerialPort p
    closeDevice (MultiModeLasers _ port) = closeSerialPort port
    availableLightSources _ =
        [LightSourceDescription (LSName "FRETLasers") True True [LSChannelName "Red", LSChannelName "Green"]]
    activateLightSource (MultiModeLasers _ port) _ chs =
        forM_ chs (\(LSChannelName n, LSIlluminationPower p) ->
            case n of
                "Red"   -> setMMLaserPower port MMRed p
                "Green" -> setMMLaserPower port MMGreen p
                _            -> throwIO $ userError "unknown laser type for MultiModeLasers")
    deactivateLightSource (MultiModeLasers _ port) =
        setMMLaserPower port MMRed 0.0 >> setMMLaserPower port MMGreen 0.0

setMMLaserPower :: SerialPort -> MMLaser -> Double -> IO ()
setMMLaserPower port laser power =
    let lCode = case laser of
                    MMRed -> fromIntegral (ord 'R')
                    MMGreen -> fromIntegral (ord 'G')
        power8 = round (power / 100.0 * 255.0)
        cmd = B.singleton lCode <> T.encodeUtf8 (T.pack (show power8)) <> B.singleton (fromIntegral (ord '\r'))
    in  serialWriteAndReadUntilChar port cmd '\r' >>= \resp ->
        case resp of
            "OK\r" -> pure ()
            e      -> throwIO $ userError ("unexpected response from MMLasers: " ++ T.unpack (T.decodeUtf8 e))
