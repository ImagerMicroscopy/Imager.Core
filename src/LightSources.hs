{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module LightSources (
    LightSourceDesc
  , LightSource
  , readAvailableLightSources
  , withLightSources
  , lookupLightSource
  , activateLightSource
  , deactivateLightSource
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
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import System.Environment
import System.FilePath
import System.Hardware.Serialport
import System.IO

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
                     | DummyLightSourceDesc {
                           dlsdName :: !Text
                       }
                     deriving (Show, Read)

data LightSource = GPIOLightSource !Text !GPIOPin !Double !GPIOHandles
                 | CoherentLightSource !Text !SerialPort
                 | LumencorLightSource !Text !SerialPort !(IORef Bool) !(IORef LumencorFilter)
                 | DummyLightSource !Text

data LumencorChannel = LCViolet | LCBlue | LCCyan | LCTeal
                     | LCGreen | LCYellow | LCRed
                       deriving (Eq)
data LumencorFilter = LCGreenFilter | LCYellowFilter
                      deriving (Eq)

instance ToJSON LightSourceDesc where
    toJSON ls = object ["name" .= lsName ls, "channels" .= lightSourceDescChannels ls,
                        "allowmultiplechannels" .= lightSourceDescAllowsMultipleChannels ls,
                        "cancontrolpower" .= lightSourceDescCanControlPower ls]
        where
          lsName :: LightSourceDesc -> Text
          lsName (GPIOLightSourceDesc name _ _) = name
          lsName (CoherentLightSourceDesc name _) = name
          lsName (LumencorLightSourceDesc name _) = name
          lsName (DummyLightSourceDesc name) = name

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
lumencorChannelFromName c = error ("unknown channel" ++ T.unpack c)

dummyLightSourceChannels :: [Text]
dummyLightSourceChannels = ["ch1", "ch2"]

readAvailableLightSources :: IO [LightSourceDesc]
readAvailableLightSources =
    getExecutablePath >>= \exePath ->
    readFile (takeDirectory exePath </> confFilename) >>=
    return . read
    where
      confFilename = "lightsources.txt"

lightSourceDescChannels :: LightSourceDesc -> [Text]
lightSourceDescChannels (LumencorLightSourceDesc _ _) = lumencorChannels
lightSourceDescChannels (DummyLightSourceDesc _) = dummyLightSourceChannels
lightSourceDescChannels _ = [""]

lightSourceDescAllowsMultipleChannels :: LightSourceDesc -> Bool
lightSourceDescAllowsMultipleChannels (LumencorLightSourceDesc _ _) = True
lightSourceDescAllowsMultipleChannels (DummyLightSourceDesc _) = True
lightSourceDescAllowsMultipleChannels _ = False

lightSourceDescCanControlPower :: LightSourceDesc -> Bool
lightSourceDescCanControlPower (GPIOLightSourceDesc _ _ _) = False
lightSourceDescCanControlPower (CoherentLightSourceDesc _ _) = True
lightSourceDescCanControlPower (LumencorLightSourceDesc _ _) = True
lightSourceDescCanControlPower (DummyLightSourceDesc _) = True

lightSourceAllowsMultipleChannels :: LightSource -> Bool
lightSourceAllowsMultipleChannels (LumencorLightSource _ _ _ _) = True
lightSourceAllowsMultipleChannels (DummyLightSource _) = True
lightSourceAllowsMultipleChannels _ = False

lightSourceHasChannel :: LightSource -> Text -> Bool
lightSourceHasChannel (LumencorLightSource _ _ _ _) ch = ch `elem` lumencorChannels
lightSourceHasChannel (DummyLightSource _) ch = ch `elem` dummyLightSourceChannels
lightSourceHasChannel _ channel | channel == "" = True
                                | otherwise = False

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
            let port = openSerial portName (defaultSerialSettings {commSpeed = CS19200})
            in CoherentLightSource name <$> port
        openLightSource (LumencorLightSourceDesc name portName) =
            let port = openSerial portName (defaultSerialSettings {commSpeed = CS115200})
            in LumencorLightSource name <$> port <*> newIORef False <*> newIORef LCGreenFilter
        openLightSource (DummyLightSourceDesc name) = putStrLn ("opened light source " ++ T.unpack name) >> return (DummyLightSource name)
        openLightSource _ = error "opening unknown type of light source"

closeLightSources :: [LightSource] -> IO ()
closeLightSources = mapM_ closeLightSource
    where
        closeLightSource (GPIOLightSource _ _ _ _) = return ()
        closeLightSource (CoherentLightSource _ port) = closeSerial port
        closeLightSource (LumencorLightSource _ port _ _) = closeSerial port
        closeLightSource (DummyLightSource name) = putStr ("closed light source " ++ T.unpack name) >> return ()

lightSourceName :: LightSource -> Text
lightSourceName (GPIOLightSource name _ _ _) = name
lightSourceName (CoherentLightSource name _) = name
lightSourceName (DummyLightSource name) = name

isKnownLightSource :: [LightSource] -> Text -> Bool
isKnownLightSource lss n = isJust $ lookupMaybeLightSource lss n

lookupLightSource :: [LightSource] -> Text -> LightSource
lookupLightSource sources name =
    case (lookupMaybeLightSource sources name) of
        Nothing -> error "invalid light source name"
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

activateLightSource :: LightSource -> [Text] -> [Double] -> IO (Either String ())
activateLightSource ls channels powers
  | (not . T.null) (validLightSourceChannelsAndPowers ls channels powers) = error "invalid light source parameters"
  | otherwise = activateLightSource' ls channels powers
  where
    activateLightSource' (GPIOLightSource _ pin delay handles) _ _ = setPinLevel handles pin High >> threadDelay (floor $ 1e6 * delay) >> return (Right ())
    activateLightSource' (DummyLightSource name) chs ps = putStrLn ("activated " ++ T.unpack name ++ " with channels " ++ show chs ++ " with powers " ++ show ps) >> return (Right ())
    activateLightSource' ls@(CoherentLightSource _ _) _ [power] = activateCoherentLightSource ls power
    activateLightSource' ls@(LumencorLightSource _ _ _ _) chs ps = activateLumencorLightSource ls (map lumencorChannelFromName chs) ps
    activateLightSource' _ _ _ = error "activating unknown light source"

activateCoherentLightSource :: LightSource -> Double -> IO (Either String ())
activateCoherentLightSource (CoherentLightSource _ port) power =
    catch (powerStr >>= sendAndReadResponse port >> sendAndReadResponse port "L=1\r" >> return (Right ()))
          (\e -> return (Left (displayException (e :: IOException))))
    where
        powerStr :: IO ByteString
        powerStr = minPowerQ >>= \minPower -> maxPowerQ >>= \maxPower ->
                  let powerVal = floor (minPower + power / 100.0 * (maxPower - minPower)) :: Int
                  in return ("P=" <> T.encodeUtf8 (T.pack $ show powerVal) <> "\r")
        minPowerQ = parseQuery "?MINLP\r"
        maxPowerQ = parseQuery "?MAXLP\r"
        parseQuery :: ByteString -> IO Double
        parseQuery q = sendAndReadResponse port q >>= return . read . filter (`elem` ('.' : ['0' .. '9'])) . T.unpack . T.decodeUtf8
        sendAndReadResponse :: SerialPort -> ByteString -> IO ByteString
        sendAndReadResponse prt msg = send port msg >> recvUntilTerminator port
        recvUntilTerminator port = readFromSerialUntilChar port 10

activateLumencorLightSource :: LightSource -> [LumencorChannel] -> [Double] -> IO (Either String ())
activateLumencorLightSource (LumencorLightSource _ port haveInitRef currFilterRef) channels powers =
    readIORef haveInitRef >>= \haveInit ->
    when (not haveInit) (   -- init RS232 and arbitrarily select the green filter
        send port lumencorEnableRS232Message >>
        changeFilter LCGreenFilter) >>
    readIORef currFilterRef >>= \currFilter ->
    when ((isJust filterForChannel) && (currFilter /= fromJust filterForChannel)) (
        changeFilter $ fromJust filterForChannel) >>
    readIORef currFilterRef >>= \possiblyUpdatedFilter ->
    send port (lumencorIntensityMessage channels powers) >>
    send port (lumencorEnableMessage channels possiblyUpdatedFilter) >>
    return (Right ())
    where
        changeFilter filter =
            send port (lumencorFilterMessage filter) >>
            threadDelay (floor (0.3 * 1.0e6)) >>
            writeIORef currFilterRef filter
        filterForChannel | LCGreen `elem` channels  = Just LCGreenFilter
                         | LCYellow `elem` channels = Just LCYellowFilter
                         | otherwise                = Nothing

deactivateLightSource :: LightSource -> IO (Either String ())
deactivateLightSource (GPIOLightSource _ pin delay handles) = setPinLevel handles pin Low >> threadDelay (floor $ 1e6 * delay) >> return (Right ())
deactivateLightSource (CoherentLightSource name port) = catch (send port "L=0\r" >> readFromSerialUntilChar port 10 >> return (Right ())) (\e -> return (Left (displayException (e :: IOException))))
deactivateLightSource (DummyLightSource name) = putStrLn ("deactivated " ++ T.unpack name) >> return (Right ())
deactivateLightSource _ = error "deactivating unknown light source"

deactivateAllLightSources :: [LightSource] -> IO (Either String ())
deactivateAllLightSources sources =
    (runExceptT . sequence . map (ExceptT . deactivateLightSource) $ sources) >>= \result ->
    case result of
        Right _ -> return (Right ())
        Left e -> return (Left e)

lumencorEnableRS232Message :: ByteString
lumencorEnableRS232Message = runPut $
    mapM_ putWord8 [0x57, 0x02, 0xFF, 0x50, 0x57, 0x03, 0xAB, 0x50]

lumencorFilterMessage :: LumencorFilter -> ByteString
lumencorFilterMessage = lumencorDisableMessage

lumencorEnableMessage :: [LumencorChannel] -> LumencorFilter -> ByteString
lumencorEnableMessage channels filter = runPut $
    mapM_ putWord8 [0x4F, enableByte, 0x50]
    where
        enableByte :: Word8
        enableByte = let channelsEnable = complement $  foldl' (\accum ch -> accum .|. (channelEnableByte ch)) 0 channels
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
lumencorChannelIntensityMessage ch p = runPut $
    mapM_ putWord8 [0x53, byte5, 0x03, byte3, byte2, byte1, 0x50]
    where
        intensityByte = round ((1.0 - p / 100.0) * 255) -- 255 means no light`
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
lumencorDisableMessage filter = runPut $
    mapM_ putWord8 [0x4F, 0xFF .&. filterSelectByte filter, 0x50]
    where
        filterSelectByte LCGreenFilter = 2^4
        filterSelectByte LCYellowFilter = 0

readFromSerialUntilChar :: SerialPort -> Word8 -> IO ByteString
readFromSerialUntilChar port c = readUntil' port c B.empty
    where
        readUntil' :: SerialPort -> Word8 -> ByteString -> IO ByteString
        readUntil' port c accum | (not $ B.null accum) && (B.last accum == c) = return accum
                                | otherwise         = recv port 100 >>= \msg ->
                                                      readUntil' port c (accum <> msg)
