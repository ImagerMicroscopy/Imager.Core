{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module LightSources where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import Data.Monoid
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
                     | DummyLightSourceDesc {
                           dlsdName :: !Text
                       }
                     deriving (Show, Read)

data LightSource = GPIOLightSource !Text !GPIOPin !Double !GPIOHandles
                 | CoherentLightSource !Text !SerialPort
                 | DummyLightSource !Text

instance ToJSON LightSourceDesc where
    toJSON ls = object ["name" .= lsName ls, "channels" .= lightSourceDescChannels ls,
                        "allowmultiplechannels" .= lightSourceDescAllowsMultipleChannels ls,
                        "cancontrolpower" .= lightSourceDescCanControlPower ls]
        where
          lsName :: LightSourceDesc -> Text
          lsName (GPIOLightSourceDesc name _ _) = name
          lsName (CoherentLightSourceDesc name _) = name
          lsName (DummyLightSourceDesc name) = name

readAvailableLightSources :: IO [LightSourceDesc]
readAvailableLightSources =
    getExecutablePath >>= \exePath ->
    readFile (takeDirectory exePath </> confFilename) >>=
    return . read
    where
      confFilename = "lightsources.txt"

lightSourceDescChannels :: LightSourceDesc -> [Text]
lightSourceDescChannels _ = [""]

lightSourceDescAllowsMultipleChannels :: LightSourceDesc -> Bool
lightSourceDescAllowsMultipleChannels _ = False

lightSourceDescCanControlPower :: LightSourceDesc -> Bool
lightSourceDescCanControlPower (GPIOLightSourceDesc _ _ _) = False
lightSourceDescCanControlPower (CoherentLightSourceDesc _ _) = True
lightSourceDescCanControlPower (DummyLightSourceDesc _) = True

lightSourceAllowsMultipleChannels :: LightSource -> Bool
lightSourceAllowsMultipleChannels _ = False

lightSourceHasChannel :: LightSource -> Text -> Bool
lightSourceHasChannel ls channel | channel == "" = True
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
        openLightSource (DummyLightSourceDesc name) = putStrLn ("opened light source " ++ T.unpack name) >> return (DummyLightSource name)
        openLightSource _ = error "opening unknown type of light source"

closeLightSources :: [LightSource] -> IO ()
closeLightSources = mapM_ closeLightSource
    where
        closeLightSource (GPIOLightSource _ _ _ _) = return ()
        closeLightSource (DummyLightSource name) = putStr ("closed light source " ++ T.unpack name) >> return ()
        closeLightSource (CoherentLightSource _ port) = closeSerial port

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
    | otherwise = T.empty

activateLightSource :: LightSource -> [Text] -> [Double] -> IO (Either String ())
activateLightSource ls channels powers
  | (not . T.null) (validLightSourceChannelsAndPowers ls channels powers) = error "invalid light source parameters"
  | otherwise = activateLightSource' ls channels powers
  where
    activateLightSource' (GPIOLightSource _ pin delay handles) _ _ = setPinLevel handles pin High >> threadDelay (floor $ 1e6 * delay) >> return (Right ())
    activateLightSource' (DummyLightSource name) [c] [p] = putStrLn ("activated " ++ T.unpack name ++ " at power " ++ show p ++ " with channel " ++ T.unpack c) >> return (Right ())
    activateLightSource' (CoherentLightSource name port) _ [power] =
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
    activateLightSource' _ _ _ = error "activating unknown light source"

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

readFromSerialUntilChar :: SerialPort -> Word8 -> IO ByteString
readFromSerialUntilChar port c = readUntil' port c B.empty
    where
        readUntil' :: SerialPort -> Word8 -> ByteString -> IO ByteString
        readUntil' port c accum | (not $ B.null accum) && (B.last accum == c) = return accum
                                | otherwise         = recv port 100 >>= \msg ->
                                                      readUntil' port c (accum <> msg)
