{-# LANGUAGE OverloadedStrings #-}
module Sutter where

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

data SutterLambda10B = SutterLambda10B !EqName ![(FName, Int)] !SerialPort

initializeSutterLambda10B :: EquipmentDescription -> IO EquipmentW
initializeSutterLambda10B (SutterLambda10BDesc name portName chs) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS128000}) (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        serialWriteByte port 238 >> serialReadUntilChar port '\r'>>
        serialWriteByte port 253 >> serialReadUntilChar port '\r' >>
        return (EquipmentW $ SutterLambda10B (EqName name) (validateFilters FName (0, 9) chs) port)

instance Equipment SutterLambda10B where
    equipmentName (SutterLambda10B n _ _) = n
    flushSerialPorts (SutterLambda10B _ _ port) = flushSerialPort port
    closeDevice (SutterLambda10B _ _ port) = closeSerialPort port
    availableFilterWheels (SutterLambda10B _ chs _) = [FilterWheelDescription (FWName "FW") (map fst chs)]
    switchToFilter (SutterLambda10B _ chs port) _ chName =
        let filterIndex = (fromIntegral . fromJust . lookup chName) chs
            speed = 3
            byte = (speed `shiftL` 4) .|. filterIndex
        in serialWrite port (B.pack [byte]) >>
           serialReadUntilChar port '\r' >> return ()
