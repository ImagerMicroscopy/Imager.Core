{-# LANGUAGE BangPatterns, OverloadedStrings, NumDecimals #-}

module LightSources (
    LightSourceDesc(..)
  , LightSource
  , readAvailableLightSources
  , withLightSources
  , lookupLightSource
  , activateLightSource
  , deactivateLightSource
  , deactivateAllLightSources
  , turnOffLightSource
  , isKnownLightSource
  , lookupMaybeLightSource
  , lookupEitherLightSource
  , validLightSourceChannelsAndPowers
  , gpioPinsNeededForLightSources
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Either
import Data.Set (Set)
import qualified Data.Set as S
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Serialize hiding(flush)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import System.Environment
import System.FilePath
import System.Hardware.Serialport hiding(timeout)
import System.IO
import System.Timeout

import GPIO
import MiscUtils

data LightSourceDesc = GPIOLightSourceDesc {
                           glsdName :: !Text
                         , glsdPin :: !GPIOPin
                         , glsdWaitDelay :: !Double
                       }
                     | CoherentLightSourceDesc {
                           clsdName :: !Text
                         , clsdSerialPortName :: !String
                       }
                     | LumencorLightSourceDesc {
                           llsdName :: !Text
                         , llsdSerialPortName :: !String
                       }
                     | AsahiLightSourceDesc {
                           alsName :: !Text
                         , alsSerialPortName :: !String
                         , alsFilters :: ![(Text, (Int, Double))]
                       }
                     | ArduinoLightSourceDesc {
                           ardName :: !Text
                         , ardSerialPortName :: !String
                         , ardChannels :: ![(Text, (Int, Double))]
                       }
                     | DummyLightSourceDesc {
                           dlsdName :: !Text
                       }
                     deriving (Show, Read)

data LightSource = GPIOLightSource !Text !GPIOPin !Double !GPIOHandles
                 | CoherentLightSource !Text !SerialPort !(IORef (Bool, Double, Double))
                 | LumencorLightSource !Text !SerialPort !(IORef Bool) !(IORef LumencorFilter)
                 | AsahiLightSource !Text ![(Text, (Int, Double))] !SerialPort
                 | ArduinoLightSource !Text ![(Text, (Int, Double))] !(IORef [(Int, Double)]) !SerialPort
                 | DummyLightSource !Text

data LumencorChannel = LCViolet | LCBlue | LCCyan | LCTeal
                     | LCGreen | LCYellow | LCRed
                       deriving (Eq)
data LumencorFilter = LCGreenFilter | LCYellowFilter
                      deriving (Eq)

data ArduinoPinState = ArduinoInput | ArduinoOutput

instance ToJSON LightSource where
    toJSON ls = object ["name" .= lightSourceName ls, "channels" .= lightSourceChannels ls,
                        "allowmultiplechannels" .= lightSourceAllowsMultipleChannels ls,
                        "cancontrolpower" .= lightSourceCanControlPower ls]

readAvailableLightSources :: IO [LightSourceDesc]
readAvailableLightSources =
    getExecutablePath >>= \exePath ->
    readFile (takeDirectory exePath </> confFilename) >>=
    return . read
    where
      confFilename = "lightsources.txt"

lightSourceCanControlPower :: LightSource -> Bool
lightSourceCanControlPower (GPIOLightSource _ _ _ _) = False
lightSourceCanControlPower (ArduinoLightSource _ _ _ _) = False
lightSourceCanControlPower _ = True

lightSourceAllowsMultipleChannels :: LightSource -> Bool
lightSourceAllowsMultipleChannels (LumencorLightSource _ _ _ _) = True
lightSourceAllowsMultipleChannels (ArduinoLightSource _ _ _ _) = True
lightSourceAllowsMultipleChannels (DummyLightSource _) = True
lightSourceAllowsMultipleChannels _ = False

lightSourceName :: LightSource -> Text
lightSourceName (GPIOLightSource name _ _ _) = name
lightSourceName (CoherentLightSource name _ _) = name
lightSourceName (LumencorLightSource name _ _ _) = name
lightSourceName (AsahiLightSource name _ _) = name
lightSourceName (ArduinoLightSource name _ _ _) = name
lightSourceName (DummyLightSource name) = name

lightSourceChannels :: LightSource -> [Text]
lightSourceChannels (LumencorLightSource _ _ _ _) = lumencorChannels
lightSourceChannels (DummyLightSource _) = dummyLightSourceChannels
lightSourceChannels (AsahiLightSource _ chs _) = map fst chs
lightSourceChannels (ArduinoLightSource _ chs _ _) = map fst chs
lightSourceChannels _ = [""]

lightSourceHasChannel :: LightSource -> Text -> Bool
lightSourceHasChannel ls ch = ch `elem` (lightSourceChannels ls)

gpioPinsNeededForLightSources :: [LightSourceDesc] -> [GPIOPin]
gpioPinsNeededForLightSources = map extractPin . filter isGPIOLightSource
    where
        extractPin = glsdPin
        isGPIOLightSource (GPIOLightSourceDesc _ _ _) = True
        isGPIOLightSource _ = False

withLightSources :: GPIOHandles -> [LightSourceDesc] -> ([LightSource] -> IO a) -> IO a
withLightSources handles descs action =
    bracket (openLightSources handles descs) closeLightSources action

openLightSources :: GPIOHandles -> [LightSourceDesc] -> IO [LightSource]
openLightSources gpioHandles descs = sequence $ map openLightSource descs
    where
        openLightSource (GPIOLightSourceDesc name pin delay) = return (GPIOLightSource name pin delay gpioHandles)
        openLightSource (CoherentLightSourceDesc name portName) =
            openSerialWithErrorMsg portName (defaultSerialSettings {commSpeed = CS19200}) >>= \port ->
            newIORef (False, 0.0, 0.0) >>= \powerRange ->
            return (CoherentLightSource name port powerRange)
        openLightSource (LumencorLightSourceDesc name portName) =
            let port = openSerialWithErrorMsg portName (defaultSerialSettings {commSpeed = CS9600})
            in LumencorLightSource name <$> port <*> newIORef False <*> newIORef LCGreenFilter
        openLightSource (AsahiLightSourceDesc name portName chs) =
            putStrLn "Connecting to Asahi lamp..." >>
            openSerialWithErrorMsg portName defaultSerialSettings >>= \port ->
            timeout 2e6 (readLampLife port) >>= \response ->
            case response of
                Nothing -> throwIO (userError "timeout communicating with Asahi lamp")
                Just v -> putStrLn ("Asahi lamp has been on " ++ show v ++ " hours (recommended lamp life 500 hours, max lamp life 1000 hours)") >>
                          return (AsahiLightSource name (validChannelNames chs (1, 8)) port)
            where
                readLampLife :: SerialPort -> IO Double
                readLampLife port =
                    send port "LIFE?\r\n" >> readFromSerialUntilChar port '\n' >>= -- response is of format "LF %d\r"
                    return . read . filter (`elem` ("01234567890." :: String)) . byteStringAsString
        openLightSource (ArduinoLightSourceDesc name portName chs) =
            putStrLn "Connecting to Arduino" >>
            let chs' = validChannelNames chs (2, 13)
            in openSerialWithErrorMsg portName (defaultSerialSettings {commSpeed = CS115200}) >>= \port -> threadDelay (floor 2e6) >> -- delay needed, otherwise the arduino won't receive the messages.
               setArduinoPinsState ArduinoOutput (map (fst . snd) chs) port >>
               newIORef [] >>= \activeSet ->
               return (ArduinoLightSource name chs activeSet port)
        openLightSource (DummyLightSourceDesc name) = putStrLn ("opened light source " ++ T.unpack name) >> return (DummyLightSource name)
        openLightSource _ = throwIO (userError "opening unknown type of light source")

validChannelNames :: [(Text, (Int, Double))] -> (Int, Int) -> [(Text, (Int, Double))]
validChannelNames chs (lowChanLim, highChanLim)
    | any (T.null . fst) chs = throw (userError "cannot have empty filter/channel names")
    | not (nodups (map fst chs) && nodups (map (fst . snd) chs)) = throw (userError "cannot have duplicate filter/channel names")
    | not $ all (\n -> within n lowChanLim highChanLim) (map (fst . snd) chs) = throw (userError "invalid light source filter/channel index")
    | not $ all (\n -> n >= 0.0) (map (snd . snd) chs) = throw (userError "negative shutter delay time")
    | otherwise = chs

closeLightSources :: [LightSource] -> IO ()
closeLightSources = mapM_ closeLightSource
    where
        closeLightSource (GPIOLightSource _ _ _ _) = return ()
        closeLightSource (CoherentLightSource _ port _) = closeSerial port
        closeLightSource (LumencorLightSource _ port _ _) = closeSerial port
        closeLightSource (AsahiLightSource _ _ port) = closeSerial port
        closeLightSource (ArduinoLightSource _ chs _ port) = setArduinoPinsState ArduinoInput (map (fst . snd) chs) port >> closeSerial port
        closeLightSource (DummyLightSource name) = putStr ("closed light source " ++ T.unpack name) >> return ()

isKnownLightSource :: [LightSource] -> Text -> Bool
isKnownLightSource lss n = isJust $ lookupMaybeLightSource lss n

lookupLightSource :: [LightSource] -> Text -> LightSource
lookupLightSource sources name =
    case (lookupMaybeLightSource sources name) of
        Nothing -> throw (userError "invalid light source name")
        Just l  -> l

lookupMaybeLightSource :: [LightSource] -> Text -> Maybe LightSource
lookupMaybeLightSource lightSources name =
    listToMaybe . filter ((==) (T.toCaseFold name) . T.toCaseFold . lightSourceName) $ lightSources

lookupEitherLightSource :: [LightSource] -> Text -> Either String LightSource
lookupEitherLightSource lightSources name =
    case (lookupMaybeLightSource lightSources name) of
        Just l -> Right l
        Nothing -> Left "no such light source"

validLightSourceChannelsAndPowers :: LightSource -> [Text] -> [Double] -> Text
validLightSourceChannelsAndPowers ls channels powers
    | length channels /= length powers = "must have same number of channels and powers"
    | (length channels > 1) && (not (lightSourceAllowsMultipleChannels ls)) = "light source does not allow multiple channels"
    | null channels = "channels cannot be empty"
    | not (all (\c -> lightSourceHasChannel ls c) channels) = "invalid channel(s)"
    | not (all (\p -> within p 0.0 100.0) powers) = "power outside valid range"
    | nub channels /= channels = "duplicate channels requested"
    | otherwise = T.empty

activateLightSource :: LightSource -> [Text] -> [Double] -> IO ()
activateLightSource ls channels powers
  | (not . T.null) (validLightSourceChannelsAndPowers ls channels powers) = throwIO (userError "invalid light source parameters")
  | otherwise = timeout timeoutDuration (activateLightSource' ls channels powers) >>= \result ->
                case result of
                    Nothing -> throwIO (userError ("timeout activating light source " ++ (T.unpack $ lightSourceName ls)))
                    Just v -> return v
  where
    activateLightSource' (GPIOLightSource _ pin delay handles) _ _ = setPinLevel handles pin High >> threadDelay (floor $ 1e6 * delay) >> return ()
    activateLightSource' (DummyLightSource name) chs ps = putStrLn ("activated " ++ T.unpack name ++ " with channels " ++ show chs ++ " with powers " ++ show ps) >> return ()
    activateLightSource' ls@(CoherentLightSource _ _ _) _ [power] = activateCoherentLightSource ls power
    activateLightSource' ls@(LumencorLightSource _ _ _ _) chs ps = activateLumencorLightSource ls (map lumencorChannelFromName chs) ps
    activateLightSource' ls@(AsahiLightSource _ _ _) [ch] [p] = activateAsahiLightSource ls ch p
    activateLightSource' ls@(ArduinoLightSource _ _ _ _) chs ps = activateArduinoLightSource ls chs ps
    activateLightSource' _ _ _ = throwIO (userError "activating unknown light source")
    timeoutDuration = floor (2.0e6)

activateCoherentLightSource :: LightSource -> Double -> IO ()
activateCoherentLightSource (CoherentLightSource _ port powerRange) power =
    powerStr >>= sendAndReadResponse >> sendAndReadResponse "L=1\r" >> return ()
    where
        powerStr :: IO ByteString
        powerStr = minMaxPower >>= \(minPower, maxPower) ->
                   let powerVal = floor (minPower + power / 100.0 * (maxPower - minPower)) :: Int
                   in return ("P=" <> T.encodeUtf8 (T.pack $ show powerVal) <> "\r")
        minMaxPower :: IO (Double, Double)
        minMaxPower = readIORef powerRange >>= \(haveRange, minP, maxP) ->
                      if (haveRange)
                      then return (minP, maxP)
                      else minPowerQ >>= \minPower -> maxPowerQ >>= \maxPower ->
                           writeIORef powerRange (True, minPower, maxPower) >>
                           sendAndReadResponse "CDRH=0\r" >> sendAndReadResponse "CW=1\r" >>
                           return (minPower, maxPower)
        minPowerQ = parseQuery "?MINLP\r"
        maxPowerQ = parseQuery "?MAXLP\r"
        parseQuery :: ByteString -> IO Double
        parseQuery q = sendAndReadResponse q >>= return . read . filter (`elem` ('.' : ['0' .. '9'])) . T.unpack . T.decodeUtf8
        sendAndReadResponse :: ByteString -> IO ByteString
        sendAndReadResponse msg = send port msg >> readFromSerialUntilChar port '\n'

activateLumencorLightSource :: LightSource -> [LumencorChannel] -> [Double] -> IO ()
activateLumencorLightSource (LumencorLightSource _ port haveInitRef currFilterRef) channels powers =
    readIORef haveInitRef >>= \haveInit ->
    when (not haveInit) (   -- init RS232 and arbitrarily select the green filter
        send port lumencorEnableRS232Message >>
        changeFilter LCGreenFilter >>
        writeIORef haveInitRef True) >>
    readIORef currFilterRef >>= \currFilter ->
    when ((isJust filterForChannel) && (currFilter /= fromJust filterForChannel)) (
         changeFilter $ fromJust filterForChannel) >>
    readIORef currFilterRef >>= \possiblyUpdatedFilter ->
    send port (lumencorIntensityMessage channels powers) >>
    send port (lumencorEnableMessage channels possiblyUpdatedFilter) >>
    return ()
    where
        changeFilter filter =
            send port (lumencorFilterMessage filter) >>
            threadDelay (floor (0.3 * 1.0e6)) >>
            writeIORef currFilterRef filter
        filterForChannel | LCGreen `elem` channels  = Just LCGreenFilter
                         | LCYellow `elem` channels = Just LCYellowFilter
                         | otherwise                = Nothing

activateAsahiLightSource :: LightSource -> Text -> Double -> IO ()
activateAsahiLightSource (AsahiLightSource _ chs port) filter power =
    case (lookup filter chs) of
        Nothing -> throwIO (userError ("missing filter " ++ T.unpack filter))
        Just (idx, _) ->
            handleAsahiMessage port "S=0\r\n" >> -- close shutter
            handleAsahiMessage port ("F=" ++ show idx ++ "\r\n") >> -- set filter
            handleAsahiMessage port ("LI=" ++ (show . toInteger . round $ power) ++ "\r\n") >> --set power
            handleAsahiMessage port "S=1\r\n"

activateArduinoLightSource :: LightSource -> [Text] -> [Double] -> IO ()
activateArduinoLightSource (ArduinoLightSource _ availChs activePinRef port) chs _ =
    case (concatMaybes $ map (\ch -> lookup ch availChs) chs) of
        Nothing -> throwIO (userError "unknown arduino channel")
        Just pins ->
            setArduinoPinsLevel port pins High >>
            modifyIORef' activePinRef (\s -> nub (s ++ pins))

deactivateLightSource :: LightSource -> IO ()
deactivateLightSource (GPIOLightSource _ pin delay handles) = setPinLevel handles pin Low >> threadDelay (floor $ 1e6 * delay)
deactivateLightSource (CoherentLightSource _ port _) = send port "L=0\r" >> readFromSerialUntilChar port '\n' >> return ()
deactivateLightSource (LumencorLightSource _ port _ currFilterRef) =
    readIORef currFilterRef >>= \currFilter ->
    send port (lumencorDisableMessage currFilter) >> return ()
deactivateLightSource (AsahiLightSource _ _ port) = handleAsahiMessage port "S=0\r\n"
deactivateLightSource (ArduinoLightSource _ chs activePinRef port) =
    readIORef activePinRef >>= \activePins ->
    setArduinoPinsLevel port activePins Low >>
    writeIORef activePinRef []
deactivateLightSource (DummyLightSource name) = putStrLn ("deactivated " ++ T.unpack name)
deactivateLightSource _ = throwIO (userError "deactivating unknown type of light source")

deactivateAllLightSources :: [LightSource] -> IO ()
deactivateAllLightSources sources = mapM_ deactivateLightSource sources

turnOffLightSource :: LightSource -> IO ()
turnOffLightSource (AsahiLightSource _ _ port) = send port "PW0\r\n" >> return ()
turnOffLightSource _ = throwIO (userError "can't turn off this light source")

dummyLightSourceChannels :: [Text]
dummyLightSourceChannels = ["ch1", "ch2"]

lumencorChannels :: [Text]
lumencorChannels = ["violet", "blue", "cyan", "teal", "green", "yellow", "red"]

lumencorChannelFromName :: Text -> LumencorChannel
lumencorChannelFromName "violet" = LCViolet
lumencorChannelFromName "blue" = LCBlue
lumencorChannelFromName "cyan" = LCCyan
lumencorChannelFromName "teal" = LCTeal
lumencorChannelFromName "green" = LCGreen
lumencorChannelFromName "yellow" = LCYellow
lumencorChannelFromName "red" = LCRed
lumencorChannelFromName c = throw (userError ("unknown channel" ++ T.unpack c))

lumencorEnableRS232Message :: ByteString
lumencorEnableRS232Message =
    B.pack [0x57, 0x02, 0xFF, 0x50, 0x57, 0x03, 0xAB, 0x50]

lumencorFilterMessage :: LumencorFilter -> ByteString
lumencorFilterMessage = lumencorDisableMessage

lumencorEnableMessage :: [LumencorChannel] -> LumencorFilter -> ByteString
lumencorEnableMessage channels filter =
    B.pack [0x4F, enableByte, 0x50]
    where
        enableByte :: Word8
        enableByte = let channelsEnable = 0x7F .&. (complement $  foldl' (\accum ch -> accum .|. (channelEnableByte ch)) 0 channels)
                     in if (filter == LCGreenFilter)
                        then channelsEnable .|. 0x10
                        else channelsEnable .&. complement 0x10
        channelEnableByte LCViolet = 2^3
        channelEnableByte LCBlue = 2^5
        channelEnableByte LCCyan = 2^2
        channelEnableByte LCTeal = 2^6
        channelEnableByte LCYellow = 2^1
        channelEnableByte LCGreen = 2^1
        channelEnableByte LCRed = 2^0

lumencorIntensityMessage :: [LumencorChannel] -> [Double] -> ByteString
lumencorIntensityMessage chs ps = mconcat (zipWith lumencorChannelIntensityMessage chs ps)

lumencorChannelIntensityMessage :: LumencorChannel -> Double -> ByteString
lumencorChannelIntensityMessage ch p =
    B.pack [0x53, byte5, 0x03, byte3, byte2, byte1, 0x50]
    where
        intensityByte = round ((1.0 - p / 100.0) * 255) -- 255 means no light
        dacAndIntensity LCViolet = (0x18, 2^0)
        dacAndIntensity LCBlue = (0x1A, 2^0)
        dacAndIntensity LCCyan = (0x18, 2^1)
        dacAndIntensity LCTeal = (0x1A, 2^1)
        dacAndIntensity LCGreen = (0x18, 2^2)
        dacAndIntensity LCYellow = (0x18, 2^2)
        dacAndIntensity LCRed = (0x18, 2^3)
        byte5 = fst (dacAndIntensity ch)
        byte3 = snd (dacAndIntensity ch)
        byte2 = (intensityByte `shiftR` 4) .|. 0xF0
        byte1 = intensityByte `shiftL` 4

lumencorDisableMessage :: LumencorFilter -> ByteString
lumencorDisableMessage filter =
    B.pack [0x4F, filterSelectByte filter, 0x50]
    where
        filterSelectByte LCGreenFilter = 0x7F
        filterSelectByte LCYellowFilter = 0x6F

handleAsahiMessage :: SerialPort -> String -> IO ()
handleAsahiMessage port ss =
    flush port >> send port (T.encodeUtf8 $ T.pack ss) >> readFromSerialUntilChar port '\n' >>= \response ->
    if (response == "OK\r\n")
    then return ()
    else throwIO (userError ("sent " ++ ss ++ "to asahi, received " ++ (T.unpack $ T.decodeUtf8 response)))

handleArduinoMessage :: SerialPort -> String -> IO ()
handleArduinoMessage port ss =
    flush port >> send port (T.encodeUtf8 . T.pack $ ss) >>
    timeout (floor 1e6) (readFromSerialUntilChar port '\r') >>= \response ->
    case response of
        Just "OK\r" -> return ()
        Just e -> throwIO (userError ("arduino responded \"" ++ T.unpack (T.decodeUtf8 e) ++ "\""))
        Nothing -> throwIO (userError "timeout communicating with the arduino")

setArduinoPinsLevel :: SerialPort -> [(Int, Double)] -> Level -> IO ()
setArduinoPinsLevel port ps level =
    let pins = map fst ps
        delays = map snd ps
        maxDelayMillis = floor ((maximum delays) * 1e3) :: Int
        actualDelays = (replicate (length delays - 1) 0) ++ [maxDelayMillis]
        levelStr = if (level == High) then "high" else "low"
    in  forM_ (zip pins actualDelays) (\(p, delay) ->
            handleArduinoMessage port ("set " ++ levelStr ++ " pin " ++ show p ++ " wait " ++ show delay ++ "\r"))

setArduinoPinsState :: ArduinoPinState -> [Int] -> SerialPort -> IO ()
setArduinoPinsState state ps port =
    forM_ ps $ \p ->
        handleArduinoMessage port ("set " ++ stateStr ++ " pin " ++ show p ++ "\r")
    where
        stateStr = case state of
                    ArduinoInput -> "input"
                    ArduinoOutput -> "output"
