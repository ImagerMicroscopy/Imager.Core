{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module PWMLaserController where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
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

import Equipment
import EquipmentTypes
import MiscUtils
import RCSerialPort

data PWMLaserController = PWMLaserController !EqName !SerialPort

initializePWMLaserControllerLightSource :: EquipmentDescription -> IO EquipmentW
initializePWMLaserControllerLightSource (PWMLaserControllerDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS9600}) (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port -> threadDelay (floor 2e6) >> -- delay needed, otherwise the arduino won't receive the messages.
        return (EquipmentW (PWMLaserController (EqName name) port))

instance Equipment PWMLaserController where
    equipmentName (PWMLaserController n _) = n
    flushSerialPorts (PWMLaserController _ p) = flushSerialPort p
    closeDevice (PWMLaserController _ port) = closeSerialPort port
    availableLightSources _ =
        [LightSourceDescription (LSName "ls") True False [LSChannelName "ch"]]
    activateLightSource (PWMLaserController _ port) _ [(_, power)] =
        let bytePower = floor (fromLSIlluminationPower power * 255.0)
        in  setPWROutputLevel port bytePower
    deactivateLightSource (PWMLaserController _ port) =
        setPWROutputLevel port 0

setPWROutputLevel :: SerialPort -> Word8 -> IO ()
setPWROutputLevel port level =
    serialWriteByteAndReadByte port level >>= \response ->
    when (response /= level) (throwIO (userError ("PWRArduino response mismatch")))
