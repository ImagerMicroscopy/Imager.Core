{-# LANGUAGE BangPatterns, OverloadedStrings, NumDecimals #-}

module LightSources (
    lightSourceName
  , lightSourceCanControlPower
  , lightSourceAllowsMultipleChannels
  , lightSourceChannels
  , lookupLightSource
  , activateLightSource
  , deactivateLightSource
  , deactivateAllLightSources
  , turnOffLightSource
  , isKnownLightSource
  , lookupMaybeLightSource
  , lookupEitherLightSource
  , validLightSourceChannelsAndPowers
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
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import System.Environment
import System.FilePath
import System.IO
import qualified System.Timeout as ST

import EquipmentMessaging
import EquipmentTypes
import MiscUtils
import RCSerialPort

data Level = High | Low 
             deriving(Eq)

lightSourceCanControlPower :: Equipment -> Bool
lightSourceCanControlPower (ArduinoLightSource _ _ _ _) = False
lightSourceCanControlPower _ = True

lightSourceAllowsMultipleChannels :: Equipment -> Bool
lightSourceAllowsMultipleChannels (LumencorLightSource _ _ _ _) = True
lightSourceAllowsMultipleChannels (ArduinoLightSource _ _ _ _) = True
lightSourceAllowsMultipleChannels (DummyLightSource _) = True
lightSourceAllowsMultipleChannels _ = False

lightSourceName :: Equipment -> Text
lightSourceName (CoherentLightSource name _ _ _) = name
lightSourceName (LumencorLightSource name _ _ _) = name
lightSourceName (AsahiLightSource name _ _) = name
lightSourceName (ArduinoLightSource name _ _ _) = name
lightSourceName (DummyLightSource name) = name

lightSourceChannels :: Equipment -> [Text]
lightSourceChannels (LumencorLightSource _ _ _ _) = lumencorChannels
lightSourceChannels (DummyLightSource _) = dummyLightSourceChannels
lightSourceChannels (AsahiLightSource _ chs _) = map fst chs
lightSourceChannels (ArduinoLightSource _ chs _ _) = map fst chs
lightSourceChannels _ = [""]

lightSourceHasChannel :: Equipment -> Text -> Bool
lightSourceHasChannel ls ch = ch `elem` (lightSourceChannels ls)

isKnownLightSource :: [Equipment] -> Text -> Bool
isKnownLightSource lss n = isJust $ lookupMaybeLightSource lss n

lookupLightSource :: [Equipment] -> Text -> Equipment
lookupLightSource sources name =
    case (lookupMaybeLightSource sources name) of
        Nothing -> throw (userError "invalid light source name")
        Just l  -> l

lookupMaybeLightSource :: [Equipment] -> Text -> Maybe Equipment
lookupMaybeLightSource lightSources name =
    listToMaybe . filter ((==) (T.toCaseFold name) . T.toCaseFold . lightSourceName) $ lightSources

lookupEitherLightSource :: [Equipment] -> Text -> Either String Equipment
lookupEitherLightSource lightSources name =
    case (lookupMaybeLightSource lightSources name) of
        Just l -> Right l
        Nothing -> Left "no such light source"

validLightSourceChannelsAndPowers :: Equipment -> [Text] -> [Double] -> Text
validLightSourceChannelsAndPowers ls channels powers
    | length channels /= length powers = "must have same number of channels and powers"
    | (length channels > 1) && (not (lightSourceAllowsMultipleChannels ls)) = "light source does not allow multiple channels"
    | null channels = "channels cannot be empty"
    | not (all (\c -> lightSourceHasChannel ls c) channels) = "invalid channel(s)"
    | not (all (\p -> within p 0.0 100.0) powers) = "power outside valid range"
    | nub channels /= channels = "duplicate channels requested"
    | otherwise = T.empty

activateLightSource :: Equipment -> [Text] -> [Double] -> IO ()
activateLightSource ls channels powers
  | (not . T.null) (validLightSourceChannelsAndPowers ls channels powers) = throwIO (userError "invalid light source parameters")
  | otherwise = ST.timeout timeoutDuration (activateLightSource' ls channels powers) >>= \result ->
                case result of
                    Nothing -> throwIO (userError ("timeout activating light source " ++ (T.unpack $ lightSourceName ls)))
                    Just v -> return v
  where
    activateLightSource' (DummyLightSource name) chs ps = putStrLn ("activated " ++ T.unpack name ++ " with channels " ++ show chs ++ " with powers " ++ show ps) >> return ()
    activateLightSource' ls@(CoherentLightSource _ _ _ _) _ [power] = activateCoherentLightSource ls power
    activateLightSource' ls@(LumencorLightSource _ _ _ _) chs ps = activateLumencorLightSource ls (map lumencorChannelFromName chs) ps
    activateLightSource' ls@(AsahiLightSource _ _ _) [ch] [p] = activateAsahiLightSource ls ch p
    activateLightSource' ls@(ArduinoLightSource _ _ _ _) chs ps = activateArduinoLightSource ls chs ps
    activateLightSource' _ _ _ = throwIO (userError "activating unknown light source")
    timeoutDuration = floor (2.0e6)

activateCoherentLightSource :: Equipment -> Double -> IO ()
activateCoherentLightSource (CoherentLightSource _ port powerRange currentPower) power =
    needToSetPower >>= \needPower ->
    when (needPower) (setPower power) >>
    sendAndReadResponse "L=1\r" >> return ()
    where
        needToSetPower :: IO Bool
        needToSetPower =
            readIORef currentPower >>= \cp ->
            case cp of
                (False, _) -> return True
                (True, p)  -> return (p /= power)
        setPower :: Double -> IO ()
        setPower p = powerStr p >>= sendAndReadResponse >>
                     writeIORef currentPower (True, p)
        powerStr :: Double -> IO ByteString
        powerStr p = minMaxPower >>= \(minPower, maxPower) ->
                    let powerVal = floor (minPower + p / 100.0 * (maxPower - minPower)) :: Int
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
        sendAndReadResponse msg = flushSerialPort port >> serialWrite port msg >> serialReadUntilChar port '\n'

activateLumencorLightSource :: Equipment -> [LumencorChannel] -> [Double] -> IO ()
activateLumencorLightSource (LumencorLightSource _ port haveInitRef currFilterRef) channels powers =
    readIORef haveInitRef >>= \haveInit ->
    when (not haveInit) (   -- init RS232 and arbitrarily select the green filter
        serialWrite port lumencorEnableRS232Message >>
        changeFilter LCGreenFilter >>
        writeIORef haveInitRef True) >>
    readIORef currFilterRef >>= \currFilter ->
    when ((isJust filterForChannel) && (currFilter /= fromJust filterForChannel)) (
         changeFilter $ fromJust filterForChannel) >>
    readIORef currFilterRef >>= \possiblyUpdatedFilter ->
    serialWrite port (lumencorIntensityMessage channels powers) >>
    serialWrite port (lumencorEnableMessage channels possiblyUpdatedFilter) >>
    return ()
    where
        changeFilter filter =
            serialWrite port (lumencorFilterMessage filter) >>
            threadDelay (floor (0.3 * 1.0e6)) >>
            writeIORef currFilterRef filter
        filterForChannel | LCGreen `elem` channels  = Just LCGreenFilter
                         | LCYellow `elem` channels = Just LCYellowFilter
                         | otherwise                = Nothing

activateAsahiLightSource :: Equipment -> Text -> Double -> IO ()
activateAsahiLightSource (AsahiLightSource _ chs port) filter power =
    case (lookup filter chs) of
        Nothing -> throwIO (userError ("missing filter " ++ T.unpack filter))
        Just (idx, _) ->
            handleAsahiMessage port "S=0\r\n" >> -- close shutter
            handleAsahiMessage port ("F=" ++ show idx ++ "\r\n") >> -- set filter
            handleAsahiMessage port ("LI=" ++ (show . toInteger . round $ power) ++ "\r\n") >> --set power
            handleAsahiMessage port "S=1\r\n"

activateArduinoLightSource :: Equipment -> [Text] -> [Double] -> IO ()
activateArduinoLightSource (ArduinoLightSource _ availChs activePinRef port) chs _ =
    case (concatMaybes $ map (\ch -> lookup ch availChs) chs) of
        Nothing -> throwIO (userError "unknown arduino channel")
        Just pins ->
            setArduinoPinsLevel port pins High >>
            modifyIORef' activePinRef (\s -> nub (s ++ pins))

deactivateLightSource :: Equipment -> IO ()
deactivateLightSource (CoherentLightSource _ port _ _) = flushSerialPort port >> serialWrite port "L=0\r" >> serialReadUntilChar port '\n' >> return ()
deactivateLightSource (LumencorLightSource _ port _ currFilterRef) =
    readIORef currFilterRef >>= \currFilter ->
    serialWrite port (lumencorDisableMessage currFilter) >> return ()
deactivateLightSource (AsahiLightSource _ _ port) = handleAsahiMessage port "S=0\r\n"
deactivateLightSource (ArduinoLightSource _ chs activePinRef port) =
    readIORef activePinRef >>= \activePins ->
    setArduinoPinsLevel port activePins Low >>
    writeIORef activePinRef []
deactivateLightSource (DummyLightSource name) = putStrLn ("deactivated " ++ T.unpack name)
deactivateLightSource _ = throwIO (userError "deactivating unknown type of light source")

deactivateAllLightSources :: [Equipment] -> IO ()
deactivateAllLightSources sources = mapM_ deactivateLightSource sources

turnOffLightSource :: Equipment -> IO ()
turnOffLightSource (AsahiLightSource _ _ port) = serialWrite port "PW0\r\n" >> return ()
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

setArduinoPinsLevel :: SerialPort -> [(Int, Double)] -> Level -> IO ()
setArduinoPinsLevel port ps level =
    let pins = map fst ps
        delays = map snd ps
        maxDelayMillis = floor ((maximum delays) * 1e3) :: Int
        actualDelays = (replicate (length delays - 1) 0) ++ [maxDelayMillis]
        levelStr = if (level == High) then "high" else "low"
    in  forM_ (zip pins actualDelays) (\(p, delay) ->
            handleArduinoMessage port ("set " ++ levelStr ++ " pin " ++ show p ++ " wait " ++ show delay ++ "\r"))
