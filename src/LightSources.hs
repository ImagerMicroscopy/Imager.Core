{-# LANGUAGE BangPatterns #-}

module LightSources where

import Data.Text (Text)

import GPIO

data LightSource = Digital !GPIOPin !GPIOHandles

activateLightSource :: LightSource -> Text -> Double -> IO ()
activateLightSource source filterName power = undefined

deactivateLightSource :: LightSource -> IO ()
deactivateLightSource source = undefined

openLightSource :: LightSource -> IO ()
openLightSource source = undefined

closeLightSource :: LightSource -> IO ()
closeLightSource = undefined
