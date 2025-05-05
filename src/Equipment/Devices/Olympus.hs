{-# LANGUAGE OverloadedStrings #-}

module Equipment.Devices.Olympus where

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

import Equipment.Equipment
import Equipment.EquipmentTypes
import Utils.FilterUtils
import Utils.MiscUtils
import RCSerialPort

data OlympusIX71Dichroic = OlympusIX71Dichroic !EqName ![(FName, Int)] !(IORef (Bool, Int)) !SerialPort

initializeOlympusIX71Dichroic :: EquipmentDescription -> IO EquipmentW
initializeOlympusIX71Dichroic (OlympusIX71DichroicDesc name portName chs) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 20000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        serialWriteAndReadUntilChar port "1LOG IN\r" '\r' >>= \response ->
        when (response /= "1LOG +\r") (
            putStrLn ("unexpected response from Olymus IX71 DM: " ++ show response) >> putStrLn "press return to close" >> getLine >> error "failed") >>
        newIORef (False, 0) >>= \currFilterRef ->
        return (EquipmentW $ OlympusIX71Dichroic (EqName name) (validateFilters FName (0, 5) chs) currFilterRef port)

instance Equipment OlympusIX71Dichroic where
    equipmentName (OlympusIX71Dichroic n _ _ _) = n
    flushSerialPorts (OlympusIX71Dichroic _ _ _ port) = flushSerialPort port
    closeDevice (OlympusIX71Dichroic _ _ _ port) = closeSerialPort port
    availableMovableComponents (OlympusIX71Dichroic _ chs _ _) =
        let filterNames = map (fromFName . fst) chs
        in  [DiscreteMovableComponent "FW" filterNames]
    moveComponent (OlympusIX71Dichroic _ chs currFilter port) [DiscreteComponentSetting _ fName] =
        let filterIndex = fromJust (lookup (FName fName) chs)
            msg = (T.encodeUtf8 . T.pack $ "1MU " ++ show (filterIndex + 1) ++ "\r")
        in  readIORef currFilter >>= \(haveInit, currFilterIdx) ->
            when ((not haveInit) || (currFilterIdx /= filterIndex)) (
                serialWriteAndReadUntilChar port msg '\r' >>= \result ->
                case result of
                    "1MU +\r" -> writeIORef currFilter (True, filterIndex) >> return ()
                    v         -> throwIO (userError ("unknown response from ix71 dichroic turret: " ++ show v)))
