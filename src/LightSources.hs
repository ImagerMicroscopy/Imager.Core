{-# LANGUAGE BangPatterns #-}

module LightSources where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import GPIO

data LightSource = Digital !GPIOPin !GPIOHandles

lookupLightSourceByName :: Text -> LightSource
lookupLightSourceByName name =
    case (lookup (T.toLower name) lightSourceMap) of
        Nothing -> error "invalid light source name"
        Just l  -> l
    where
        lightSourceMap :: [(Text, LightSource)]
        lightSourceMap = map (\(n, l) -> (T.toLower n, l)) undefined

activateLightSource :: LightSource -> Text -> Double -> IO ()
activateLightSource source filterName power = undefined

deactivateLightSource :: LightSource -> IO ()
deactivateLightSource source = undefined

openLightSource :: LightSource -> IO ()
openLightSource source = undefined

closeLightSource :: LightSource -> IO ()
closeLightSource = undefined
