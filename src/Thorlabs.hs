{-# LANGUAGE OverloadedStrings #-}
module Thorlabs where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Either
import Data.IORef
import Data.List
import Data.Maybe
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Timeout as ST

import Equipment
import EquipmentMessaging
import EquipmentTypes
import FilterUtils
import MiscUtils
import RCSerialPort

data ThorlabsFW103H = ThorlabsFW103H !FWName ![(Text, Int)] !SerialPort
data ThorlabsFW102C = ThorlabsFW102C !FWName ![(Text, Int)] !SerialPort

initializeThorlabsFW130H :: EquipmentDescription -> IO EquipmentW
initializeThorlabsFW130H (ThorlabsFW103HDesc name portName chs) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        putStr "initializing Thorlabs FW103H filter wheel..." >>
        forM_ fw103HStartupMessages (\msg -> serialWrite port msg >> threadDelay (floor 25e3)) >>
        serialWrite port fw103HStopUpdatesMessage >> serialWrite port fw103HMoveHomeMessage >>
        fw103HWaitUntilHomingStops port >>
        putStrLn "done!" >>
        return (EquipmentW $ ThorlabsFW103H (FWName name) (validateFilters id (0, 5) chs) port)

initializeThorlabsFW102C :: EquipmentDescription -> IO EquipmentW
initializeThorlabsFW102C (ThorlabsFW102CDesc name portName chs) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 30000) SerialPortNoDebug
    in  EquipmentW <$> (ThorlabsFW102C (FWName name) (validateFilters id (0, 5) chs) <$> openSerialPort portName serialSettings)

instance Equipment ThorlabsFW103H where
    equipmentName _ = (EqName "ThorlabsFW103H")
    closeDevice (ThorlabsFW103H _ _ port) = closeSerialPort port
    availableFilterWheels (ThorlabsFW103H n chs _) = [(n, map fst chs)]
    switchToFilter (ThorlabsFW103H _ chs port) _ chName =
        let filterIndex = fromJust (lookup chName chs)
            wheelPos = (409600 `div` 6) * filterIndex -- Thorlabs:  1 turn represents 360 degrees which is 409600 micro steps
        in  flushSerialPort port >> serialWrite port (fw103HMoveAbsoluteMessage wheelPos) >>
            fw103HWaitUntilMotionStops port wheelPos


instance Equipment ThorlabsFW102C where
    equipmentName _ = (EqName "ThorlabsFW102C")
    closeDevice (ThorlabsFW102C _ _ port) = closeSerialPort port
    availableFilterWheels (ThorlabsFW102C n chs _) = [(n, map fst chs)]
    switchToFilter (ThorlabsFW102C _ chs port) _ chName =
        let filterIndex = fromJust (lookup chName chs)
        in flushSerialPort port >> serialWrite port (T.encodeUtf8 . T.pack $ "pos=" ++ show filterIndex) >>
            serialReadUntilChar port '>' >> return ()
