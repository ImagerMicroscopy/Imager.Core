{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Asahi where

import Control.Exception
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
import FilterUtils
import MiscUtils
import RCSerialPort

data AsahiLightSource = AsahiLightSource !Text ![(Text, Int)] !SerialPort

initializeAsahiLightSource :: EquipmentDescription -> IO EquipmentW
initializeAsahiLightSource (AsahiLightSourceDesc name portName chs) =
    let serialSettings = RCSerialPortSettings defaultSerialSettings (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        readLampLife port >>= \life ->
        putStrLn ("Asahi lamp has been on " ++ show life ++ " hours (recommended lamp life 500 hours, max lamp life 1000 hours)") >>
                  return (EquipmentW $ AsahiLightSource name (validateFilters (1, 8) chs) port)
    where
        readLampLife :: SerialPort -> IO Double
        readLampLife port =
            flushSerialPort port >> serialWrite port "LIFE?\r\n" >> serialReadUntilChar port '\n' >>= -- response is of format "LF %d\r"
            return . read . filter (`elem` ("01234567890." :: String)) . byteStringAsString

instance Equipment AsahiLightSource where
    equipmentName _ = (EqName "Asahi lamp")
    closeDevice (AsahiLightSource _ _ port) = closeSerialPort port
    availableLightSources (AsahiLightSource n chs _) =
        [LightSourceDescription n True False (map fst chs)]
    activateLightSource (AsahiLightSource _ chs port) _ ((filter, power) : _) =
        case (lookup filter chs) of
            Nothing -> throwIO (userError ("missing filter " ++ T.unpack filter))
            Just idx ->
                handleAsahiMessage port "S=0\r\n" >> -- close shutter
                handleAsahiMessage port ("F=" ++ show idx ++ "\r\n") >> -- set filter
                handleAsahiMessage port ("LI=" ++ (show . toInteger . round $ power) ++ "\r\n") >> --set power
                handleAsahiMessage port "S=1\r\n"
    deactivateLightSource (AsahiLightSource _ _ port) = handleAsahiMessage port "S=0\r\n"

handleAsahiMessage :: SerialPort -> String -> IO ()
handleAsahiMessage port ss =
    flushSerialPort port >> serialWrite port (T.encodeUtf8 $ T.pack ss) >> serialReadUntilChar port '\n' >>= \response ->
    if (response == "OK\r\n")
    then return ()
    else throwIO (userError ("sent " ++ ss ++ "to asahi, received " ++ (T.unpack $ T.decodeUtf8 response)))
