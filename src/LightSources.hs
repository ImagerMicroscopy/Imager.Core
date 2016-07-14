{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module LightSources where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Hardware.Serialport
import System.IO

import GPIO

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
                     deriving (Show)

data LightSource = GPIOLightSource !Text !GPIOPin !Double !GPIOHandles
                 | CoherentLightSource !Text !SerialPort
                 | DummyLightSource !Text

instance ToJSON LightSourceDesc where
    toJSON (GPIOLightSourceDesc name _ _) = object ["name" .= name, "channels" .= ([] :: [Text])]
    toJSON (CoherentLightSourceDesc name _) = object ["name" .= name, "channels" .= ([] :: [Text])]
    toJSON (DummyLightSourceDesc name) = object ["name" .= name, "channels" .= ([] :: [Text])]

availableLightSources :: [LightSourceDesc]
availableLightSources = [GPIOLightSourceDesc "561 nm" Pin3 0.01, GPIOLightSourceDesc "488 nm" Pin4 0.01,
                         GPIOLightSourceDesc "fluorescence" Pin2 0.01, GPIOLightSourceDesc "DT-2-GS" Pin17 0.025]
--availableLightSources = [DummyLightSourceDesc "dummy1", DummyLightSourceDesc "dummy2"]

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

activateLightSource :: LightSource -> Text -> Double -> IO (Either String ())
activateLightSource (GPIOLightSource _ pin delay handles) _ _ = setPinLevel handles pin High >> threadDelay (floor $ 1e6 * delay) >> return (Right ())
activateLightSource (CoherentLightSource name port) _ power =
    catch ((++) "P=" <$> powermW >>= \pstr -> send port (T.encodeUtf8 $ T.pack (pstr ++ "\r" ++ "L=1\r")) >> return (Right ()))
        (\e -> return (Left (displayException (e :: IOException))))
    where
        powermW :: IO String
        powermW = flush port >> send port "?MINLP" >> T.unpack . T.decodeUtf8 <$> recv port 100 >>= return . fst . head . (reads :: String -> [(Double, String)]) >>= \minPower ->
                  flush port >> send port "?MAXLP" >> T.unpack . T.decodeUtf8 <$> recv port 100 >>= return . fst . head . (reads :: String -> [(Double, String)]) >>= \maxPower ->
                  return $ show (minPower + power * (maxPower - minPower) / 100.0)
activateLightSource (DummyLightSource name) c p = putStrLn ("activated " ++ T.unpack name ++ " at power " ++ show p ++ " with channel " ++ T.unpack c) >> return (Right ())
activateLightSource _ _ _ = error "activating unknown light source"

deactivateLightSource :: LightSource -> IO (Either String ())
deactivateLightSource (GPIOLightSource _ pin delay handles) = setPinLevel handles pin Low >> threadDelay (floor $ 1e6 * delay) >> return (Right ())
deactivateLightSource (CoherentLightSource name port) = catch (send port "L=0\r" >> return (Right ())) (\e -> return (Left (displayException (e :: IOException))))
deactivateLightSource (DummyLightSource name) = putStrLn ("deactivated " ++ T.unpack name) >> return (Right ())
deactivateLightSource _ = error "deactivating unknown light source"

deactivateAllLightSources :: [LightSource] -> IO (Either String ())
deactivateAllLightSources sources =
    (runExceptT . sequence . map (ExceptT . deactivateLightSource) $ sources) >>= \result ->
    case result of
        Right _ -> return (Right ())
        Left e -> return (Left e)
