{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module LightSources where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import GPIO

data LightSourceDesc = GPIOLightSourceDesc {
                           glsdName :: !Text
                         , glsdPin :: !GPIOPin
                       }
                     | CoherentLightSourceDesc {
                           clsdName :: !Text
                         , clsdSerialPortName :: !String
                       }
                     deriving (Show)

data LightSource = GPIOLightSource !Text !GPIOPin !GPIOHandles
                 | CoherentLightSource !Text !Int -- needs to be serial port handle

instance ToJSON LightSourceDesc where
    toJSON (GPIOLightSourceDesc name _) = object ["name" .= name]
    toJSON (CoherentLightSourceDesc name _) = object ["name" .= name]

availableLightSources :: [LightSourceDesc]
availableLightSources = undefined

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
        openLightSource _ = undefined

closeLightSources :: [LightSource] -> IO ()
closeLightSources = mapM_ closeLightSource
    where
        closeLightSource (GPIOLightSource _ _ _) = return ()
        closeLightSource (CoherentLightSource _ _) = undefined

lightSourceName :: LightSource -> Text
lightSourceName (GPIOLightSource name _ _) = name
lightSourceName (CoherentLightSource name _) = name

lookupLightSource :: [LightSource] -> Text -> LightSource
lookupLightSource sources name =
    case (lookupMaybeLightSource sources name) of
        Nothing -> error "invalid light source name"
        Just l  -> l

lookupMaybeLightSource :: [LightSource] -> Text -> Maybe LightSource
lookupMaybeLightSource lightSources name =
    listToMaybe . filter ((==) (T.toCaseFold name) . T.toCaseFold . lightSourceName) $ lightSources 

activateLightSource :: LightSource -> Text -> Double -> IO ()
activateLightSource (GPIOLightSource _ pin handles) _ _ = setPinLevel handles pin High
activateLightSource _ _ _ = undefined

deactivateLightSource :: LightSource -> IO ()
deactivateLightSource (GPIOLightSource _ pin handles) = setPinLevel handles pin Low
deactivateLightSource _ = undefined

deactivateAllLightSources :: [LightSource] -> IO ()
deactivateAllLightSources sources = sequence (map deactivateLightSource sources) >> return ()
