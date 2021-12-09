{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Equipment.Devices.PWMLaserController where

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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T
import qualified Data.Text.Format as LT
import Data.Word

import Equipment.Equipment
import Equipment.EquipmentTypes
import Utils.MiscUtils
import RCSerialPort

data PWMLaserController = PWMLaserController !EqName !SerialPort !(Map Text Int)

initializePWMLaserControllerLightSource :: EquipmentDescription -> IO EquipmentW
initializePWMLaserControllerLightSource (PWMLaserControllerDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port -> threadDelay (floor 2e6) >> -- delay needed, otherwise the arduino won't receive the messages.
        queryLaserNames port >>= \lNames ->
        let nameMap = M.fromList (zip lNames [1..])
        in return (EquipmentW (PWMLaserController (EqName name) port nameMap))

instance Equipment PWMLaserController where
    equipmentName (PWMLaserController n _ _) = n
    flushSerialPorts (PWMLaserController _ port _) = flushSerialPort port
    closeDevice (PWMLaserController _ port _) = closeSerialPort port
    availableLightSources (PWMLaserController n _ lNames) =
        [LightSourceDescription (LSName "PWM") True True (map (LSChannelName . fst) (M.toList lNames))]
    activateLightSource (PWMLaserController _ port lNames) _ chs =
        forM_ chs (\(LSChannelName ch, LSIlluminationPower power) ->
            let bytePower = floor ((100.0 - power) / 100.0 * 255.0)
            in  setLaserPower port lNames ch bytePower)
    deactivateLightSource (PWMLaserController _ port lMap) =
        forM_ (M.keys lMap) (\lName -> setLaserPower port lMap lName 0)

queryLaserNames :: SerialPort -> IO [Text]
queryLaserNames port =
    serialWriteAndReadUntilChar port "laser names" '\r' >>= (return . T.splitOn ";" . T.dropEnd 1 . T.decodeUtf8)

setLaserPower :: SerialPort -> Map Text Int -> Text -> Word8 -> IO ()
setLaserPower port lMap lName power =
    let laserNum = fromJust (M.lookup lName lMap)
        cmd = T.encodeUtf8 . LT.toStrict $ LT.format "set laser {} to {}\r" [laserNum, fromIntegral power]
    in  serialWriteAndReadUntilChar port cmd '\r' >>= \response ->
        case response of
            "OK\r" -> return ()
            e      -> throwIO (userError ("unexpected response " ++ show e ++ " from PWM Laser Controller"))
