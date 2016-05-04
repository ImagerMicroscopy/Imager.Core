module LightSources
where

import Data.Text (Text)

import GPIO

data LightSource = Digital GPIOPin
                 deriving (Show)

activateLightSource :: LightSource -> Text -> Double -> IO ()
activateLightSource source filterName power = undefined

deactivateLightSource :: LightSource -> IO ()
deactivateLightSource source = undefined
