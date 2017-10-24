{-# LANGUAGE OverloadedStrings #-}
module EquipmentInitialization
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment
import System.FilePath

import EquipmentMessaging
import EquipmentTypes
import MiscUtils
import RCSerialPort

readAvailableEquipment :: IO [EquipmentDescription]
readAvailableEquipment =
    getExecutablePath >>= \exePath ->
    readFile (takeDirectory exePath </> confFilename) >>=
    return . read
    where
        confFilename = "equipment.txt"

withEquipment :: [EquipmentDescription] -> ([Equipment] -> IO ()) -> IO ()
withEquipment descs action =
    bracket (initializeEquipment descs) closeEquipment action

initializeEquipment :: [EquipmentDescription] -> IO [Equipment]
initializeEquipment = mapM initializeDevice

closeEquipment :: [Equipment] -> IO ()
closeEquipment = mapM_ closeDevice

initializeDevice :: EquipmentDescription -> IO Equipment
initializeDevice (CoherentLightSourceDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        newIORef (False, 0.0, 0.0) >>= \powerRange ->
        newIORef (False, 0.0) >>= \currentPower ->
        return (CoherentLightSource name port powerRange currentPower)
initializeDevice (LumencorLightSourceDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS9600}) (TimeoutMillis 10000) SerialPortNoDebug
        port = openSerialPort portName serialSettings
    in  LumencorLightSource name <$> port <*> newIORef False <*> newIORef LCGreenFilter
initializeDevice (AsahiLightSourceDesc name portName chs) =
    let serialSettings = RCSerialPortSettings defaultSerialSettings (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        readLampLife port >>= \life ->
        putStrLn ("Asahi lamp has been on " ++ show life ++ " hours (recommended lamp life 500 hours, max lamp life 1000 hours)") >>
                  return (AsahiLightSource name (validChannelNames chs (1, 8)) port)
    where
        readLampLife :: SerialPort -> IO Double
        readLampLife port =
            flushSerialPort port >> serialWrite port "LIFE?\r\n" >> serialReadUntilChar port '\n' >>= -- response is of format "LF %d\r"
            return . read . filter (`elem` ("01234567890." :: String)) . byteStringAsString
initializeDevice (ArduinoLightSourceDesc name portName chs) =
    putStrLn "Connecting to Arduino" >>
    let chs' = validChannelNames chs (2, 13)
        serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port -> threadDelay (floor 2e6) >> -- delay needed, otherwise the arduino won't receive the messages.
        setArduinoPinsState ArduinoOutput (map (fst . snd) chs) port >>
        newIORef [] >>= \activeSet ->
        return (ArduinoLightSource name chs activeSet port)
initializeDevice (DummyLightSourceDesc name) = putStrLn ("opened light source " ++ T.unpack name) >> return (DummyLightSource name)
initializeDevice (ThorlabsFW103HDesc name portName chs) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        putStr "initializing Thorlabs FW103H filter wheel..." >>
        forM_ fw103HStartupMessages (\msg -> serialWrite port msg >> threadDelay (floor 25e3)) >>
        serialWrite port fw103HStopUpdatesMessage >> serialWrite port fw103HMoveHomeMessage >>
        fw103HWaitUntilHomingStops port >>
        putStrLn "done!" >>
        return (ThorlabsFW103H name (validateChannels (0, 5) chs) port)
initializeDevice (ThorlabsFW102CDesc name portName chs) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 30000) SerialPortNoDebug
    in  ThorlabsFW102C name (validateChannels (0, 5) chs) <$> openSerialPort portName serialSettings
initializeDevice (SutterLambda10BDesc name portName chs) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS128000}) (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        serialWriteByte port 238 >> serialReadUntilChar port '\r'>>
        serialWriteByte port 253 >> serialReadUntilChar port '\r' >>
        return (SutterLambda10B name (validateChannels (0, 9) chs) port)
initializeDevice (OlympusIX71DichroicDesc name portName chs) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 20000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        putStr "initializing IX71 motorized dichroic..." >>
        serialWrite port "1LOG IN\r" >> serialReadUntilChar port '\r' >>= \response ->
        when (response /= "1LOG +\r") (
            putStrLn ("unexpected response from Olymus IX71 DM: " ++ show response) >> putStrLn "press return to close" >> getLine >> error "failed") >>
        putStrLn "done!" >> newIORef (False, 0) >>= \currFilterRef ->
        return (OlympusIX71Dichroic name (validateChannels (0, 5) chs) currFilterRef port)
initializeDevice (DummyFilterWheelDesc name chs) =
    putStrLn ("Opened dummy filter wheel " ++ T.unpack name ++ " with filters " ++ show chs) >> return (DummyFilterWheel name (validateChannels (0,5) chs))
initializeDevice (PriorDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS9600}) (TimeoutMillis 30000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        serialWrite port "COMP 1\r" >> serialReadUntilChar port '\r' >>= \resp ->
        if ((resp /= "0\r") && (resp /= "R\r"))
        then throwIO (userError "unexpected reply from prior stage")
        else putStrLn "Connected to Prior stage" >> PriorStage name <$> newMVar port
initializeDevice (DummyStageDesc name) =
    putStrLn ("dummy stage " ++ (T.unpack name) ++ " open") >>
    return (DummyStage name)
initializeDevice (RobottorDesc name ip port) = return (Robottor name ip port)
initializeDevice _ = error "unknown type of device description"

closeDevice :: Equipment -> IO ()
closeDevice (CoherentLightSource _ port _ _) = closeSerialPort port
closeDevice (LumencorLightSource _ port _ _) = closeSerialPort port
closeDevice (AsahiLightSource _ _ port) = closeSerialPort port
closeDevice (ArduinoLightSource _ chs _ port) = setArduinoPinsState ArduinoInput (map (fst . snd) chs) port >> closeSerialPort port
closeDevice (DummyLightSource name) = putStr ("closed light source " ++ T.unpack name) >> return ()
closeDevice (ThorlabsFW103H _ _ port) = closeSerialPort port
closeDevice (ThorlabsFW102C _ _ port) = closeSerialPort port
closeDevice (SutterLambda10B _ _ port) = closeSerialPort port
closeDevice (OlympusIX71Dichroic _ _ _ port) = closeSerialPort port
closeDevice (DummyFilterWheel name _) = putStrLn ("Closed filter wheel " ++ T.unpack name)
closeDevice (PriorStage _ portVar) = withMVar portVar $ (\port -> closeSerialPort port)
closeDevice (DummyStage n) = putStrLn ("dummy stage " ++ (T.unpack n) ++ " closed")
closeDevice (Robottor _ ip port) = return ()

validChannelNames :: [(Text, (Int, Double))] -> (Int, Int) -> [(Text, (Int, Double))]
validChannelNames chs (lowChanLim, highChanLim)
    | any (T.null . fst) chs = throw (userError "cannot have empty filter/channel names")
    | not (nodups (map fst chs) && nodups (map (fst . snd) chs)) = throw (userError "cannot have duplicate filter/channel names")
    | not $ all (\n -> within n lowChanLim highChanLim) (map (fst . snd) chs) = throw (userError "invalid light source filter/channel index")
    | not $ all (\n -> n >= 0.0) (map (snd . snd) chs) = throw (userError "negative shutter delay time")
    | otherwise = chs

validateChannels :: (Int, Int) -> [(Text, Int)] -> [(Text, Int)]
validateChannels idxLimits chs
    | haveDuplicates chs = error ("duplicate channels in " ++ show chs)
    | invalidFilterIndices idxLimits chs = error ("invalid filter indices in "  ++ show chs)
    | invalidFilterNames chs = error ("invalid filter names in " ++ show chs)
    | otherwise = chs
    where
        haveDuplicates chs = not ((nodups (map fst chs)) && (nodups (map snd chs)))
        nodups xs = (nub xs) == xs
        invalidFilterIndices (minIdx, maxIdx) = any (\(_, i) -> not (within i minIdx maxIdx))
        invalidFilterNames = any (\(n, _) -> T.null n)

setArduinoPinsState :: ArduinoPinState -> [Int] -> SerialPort -> IO ()
setArduinoPinsState state ps port =
    forM_ ps $ \p ->
        handleArduinoMessage port ("set " ++ stateStr ++ " pin " ++ show p ++ "\r")
    where
        stateStr = case state of
                    ArduinoInput -> "input"
                    ArduinoOutput -> "output"
