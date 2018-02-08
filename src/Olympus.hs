{-# LANGUAGE OverloadedStrings #-}
module Olympus where

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

data OlympusIX71Dichroic = OlympusIX71Dichroic !FWName ![(FName, Int)] !(IORef (Bool, Int)) !SerialPort

initializeOlympusIX71Dichroic :: EquipmentDescription -> IO EquipmentW
initializeOlympusIX71Dichroic (OlympusIX71DichroicDesc name portName chs) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 20000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        putStr "initializing IX71 motorized dichroic..." >>
        serialWrite port "1LOG IN\r" >> serialReadUntilChar port '\r' >>= \response ->
        when (response /= "1LOG +\r") (
            putStrLn ("unexpected response from Olymus IX71 DM: " ++ show response) >> putStrLn "press return to close" >> getLine >> error "failed") >>
        putStrLn "done!" >> newIORef (False, 0) >>= \currFilterRef ->
        return (EquipmentW $ OlympusIX71Dichroic (FWName name) (validateFilters FName (0, 5) chs) currFilterRef port)

instance Equipment OlympusIX71Dichroic where
    equipmentName _ = (EqName "Olympus IX71 Dichroic")
    closeDevice (OlympusIX71Dichroic _ _ _ port) = closeSerialPort port
    availableFilterWheels (OlympusIX71Dichroic n chs _ _) = [(n, map fst chs)]
    switchToFilter (OlympusIX71Dichroic _ chs currFilter port) _ chName =
        let filterIndex = fromJust (lookup chName chs)
        in  readIORef currFilter >>= \(haveInit, currFilterIdx) ->
            when ((not haveInit) || (currFilterIdx /= filterIndex)) (
                flushSerialPort port >> serialWrite port (T.encodeUtf8 . T.pack $ "1MU " ++ show (filterIndex + 1) ++ "\r") >>
                serialReadUntilChar port '\r' >>= \result ->
                case result of
                    "1MU +\r" -> writeIORef currFilter (True, filterIndex) >> return ()
                    v         -> throwIO (userError ("unknown response from ix71 dichroic turret: " ++ show v)))
