{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Equipment.Devices.Arduino where

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

import Equipment.Equipment
import Equipment.EquipmentTypes
import Utils.MiscUtils
import RCSerialPort

data ArduinoLightSource = ArduinoLightSource !EqName ![(LSChannelName, (Int, Double))] !(IORef [(Int, Double)]) !SerialPort

initializeArduinoLightSource :: EquipmentDescription -> IO EquipmentW
initializeArduinoLightSource (ArduinoLightSourceDesc name portName chs) =
    let chs' = validArduinoChannelNames chs (2, 13)
        serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port -> threadDelay (floor 2e6) >> -- delay needed, otherwise the arduino won't receive the messages.
        setArduinoPinsState ArduinoOutput (map (fst . snd) chs) port >>
        newIORef [] >>= \activeSet ->
        return (EquipmentW (ArduinoLightSource (EqName name) (mapFirst LSChannelName chs) activeSet port))

instance Equipment ArduinoLightSource where
    equipmentName (ArduinoLightSource n _ _ _) = n
    flushSerialPorts (ArduinoLightSource _ _ _ p) = flushSerialPort p
    closeDevice (ArduinoLightSource _ chs _ port) =
        setArduinoPinsState ArduinoInput (map (fst . snd) chs) port >> closeSerialPort port
    availableLightSources (ArduinoLightSource _ chs _ _) =
        [LightSourceDescription (LSName "ls") False True (map fst chs)]
    activateLightSource (ArduinoLightSource _ availChs activePinRef port) _ chs =
        case (concatMaybes $ map ((\ch -> lookup ch availChs) . fst) chs) of
            Nothing -> throwIO (userError "unknown arduino channel")
            Just pins ->
                setArduinoPinsLevel port pins High >>
                modifyIORef' activePinRef (\s -> nub (s ++ pins))
    deactivateLightSource (ArduinoLightSource _ chs activePinRef port) =
        readIORef activePinRef >>= \activePins ->
        setArduinoPinsLevel port activePins Low >>
        writeIORef activePinRef []

data Level = High | Low deriving(Eq)
data ArduinoPinState = ArduinoInput | ArduinoOutput

setArduinoPinsState :: ArduinoPinState -> [Int] -> SerialPort -> IO ()
setArduinoPinsState state ps port =
    forM_ ps $ \p ->
        handleArduinoMessage port ("set " ++ stateStr ++ " pin " ++ show p ++ "\r")
    where
        stateStr = case state of
                    ArduinoInput -> "input"
                    ArduinoOutput -> "output"

handleArduinoMessage :: SerialPort -> String -> IO ()
handleArduinoMessage port ss =
    serialWriteAndReadUntilChar port (T.encodeUtf8 . T.pack $ ss) '\r' >>= \response ->
    case response of
        "OK\r" -> return ()
        e -> throwIO (userError ("arduino responded \"" ++ T.unpack (T.decodeUtf8 e) ++ "\""))

validArduinoChannelNames :: [(Text, (Int, Double))] -> (Int, Int) -> [(Text, (Int, Double))]
validArduinoChannelNames chs (lowChanLim, highChanLim)
    | any (T.null . fst) chs = throw (userError "cannot have empty filter/channel names")
    | not (nodups (map fst chs) && nodups (map (fst . snd) chs)) = throw (userError "cannot have duplicate filter/channel names")
    | not $ all (\n -> within n lowChanLim highChanLim) (map (fst . snd) chs) = throw (userError "invalid light source filter/channel index")
    | not $ all (\n -> n >= 0.0) (map (snd . snd) chs) = throw (userError "negative shutter delay time")
    | otherwise = chs

setArduinoPinsLevel :: SerialPort -> [(Int, Double)] -> Level -> IO ()
setArduinoPinsLevel port ps level =
    let pins = map fst ps
        delays = map snd ps
        maxDelayMillis = floor ((maximum delays) * 1e3) :: Int
        actualDelays = (replicate (length delays - 1) 0) ++ [maxDelayMillis]
        levelStr = if (level == High) then "high" else "low"
    in  forM_ (zip pins actualDelays) (\(p, delay) ->
            handleArduinoMessage port ("set " ++ levelStr ++ " pin " ++ show p ++ " wait " ++ show delay ++ "\r"))
