{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module LightSources where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import System.Hardware.Serialport
import System.IO

import GPIO

data LightSourceDesc = GPIOLightSourceDesc {
                           glsdName :: !Text
                         , glsdPin :: !GPIOPin
                       }
                     | CoherentLightSourceDesc {
                           clsdName :: !Text
                         , clsdSerialPortName :: !String
                       }
                     | DummyLightSourceDesc {
                           dlsdName :: !Text
                       }
                     deriving (Show)

data LightSource = GPIOLightSource !Text !GPIOPin !GPIOHandles
                 | CoherentLightSource !Text !SerialPort
                 | DummyLightSource !Text

instance ToJSON LightSourceDesc where
    toJSON (GPIOLightSourceDesc name _) = object ["name" .= name, "channels" .= ([] :: [Text])]
    toJSON (CoherentLightSourceDesc name _) = object ["name" .= name, "channels" .= ([] :: [Text])]
    toJSON (DummyLightSourceDesc name) = object ["name" .= name, "channels" .= ([] :: [Text])]

availableLightSources :: [LightSourceDesc]
availableLightSources = [DummyLightSourceDesc "dummy1", DummyLightSourceDesc "dummy2"]

gpioPinsNeededForLightSources :: [LightSourceDesc] -> [GPIOPin]
gpioPinsNeededForLightSources = map extractPin . filter isGPIOLightSource
    where
        extractPin = glsdPin
        isGPIOLightSource (GPIOLightSourceDesc _ _) = True
        isGPIOLightSource _ = False

withLightSources :: GPIOHandles -> [LightSourceDesc] -> ([LightSource] -> IO a) -> IO a
withLightSources handles descs action =
    bracket (openLightSources handles descs) closeLightSources action

openLightSources :: GPIOHandles -> [LightSourceDesc] -> IO [LightSource]
openLightSources gpioHandles descs = sequence $ map openLightSource descs
    where
        openLightSource (GPIOLightSourceDesc name pin) = return (GPIOLightSource name pin gpioHandles)
        openLightSource (CoherentLightSourceDesc name portName) =
            let port = openSerial portName (defaultSerialSettings {commSpeed = CS19200})
            in CoherentLightSource name <$> port
        openLightSource (DummyLightSourceDesc name) = putStrLn ("opened light source " ++ T.unpack name) >> return (DummyLightSource name)
        openLightSource _ = error "opening unknown type of light source"

closeLightSources :: [LightSource] -> IO ()
closeLightSources = mapM_ closeLightSource
    where
        closeLightSource (GPIOLightSource _ _ _) = return ()
        closeLightSource (DummyLightSource name) = putStr ("closed light source " ++ T.unpack name) >> return ()
        closeLightSource (CoherentLightSource _ _) = undefined

lightSourceName :: LightSource -> Text
lightSourceName (GPIOLightSource name _ _) = name
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
activateLightSource (GPIOLightSource _ pin handles) _ _ = setPinLevel handles pin High >> return (Right ())
activateLightSource (CoherentLightSource name port) = catch (send port "L=1\r" >> return (Right ())) (\e -> return (Left (displayException (e :: IOException))))
activateLightSource (DummyLightSource name) c p = putStrLn ("activated " ++ T.unpack name ++ " at power " ++ show p ++ " with channel " ++ T.unpack c) >> return (Right ())
activateLightSource _ _ _ = undefined

deactivateLightSource :: LightSource -> IO (Either String ())
deactivateLightSource (GPIOLightSource _ pin handles) = setPinLevel handles pin Low >> return (Right ())
deactivateLightSource (CoherentLightSource name port) = catch (send port "L=0\r" >> return (Right ())) (\e -> return (Left (displayException (e :: IOException))))
deactivateLightSource (DummyLightSource name) = putStrLn ("deactivated " ++ T.unpack name) >> return (Right ())
deactivateLightSource _ = undefined

deactivateAllLightSources :: [LightSource] -> IO (Either String ())
deactivateAllLightSources sources =
    (runExceptT . sequence . map (ExceptT . deactivateLightSource) $ sources) >>= \result ->
    case result of
        Right _ -> return (Right ())
        Left e -> return (Left e)
