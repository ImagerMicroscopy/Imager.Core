{-# LANGUAGE BangPatterns #-}

module LightSources where

import Control.Monad
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import GPIO

data LightSource = Digital !GPIOPin !GPIOHandles

lightSourceMap :: [(Text, LightSource)]
lightSourceMap = map (\(n, l) -> (T.toLower n, l)) undefined

allLightSources :: [LightSource]
allLightSources = map snd lightSourceMap

lookupLightSourceByName :: Text -> LightSource
lookupLightSourceByName name =
    case (lookupMaybeLightSourceByName name) of
        Nothing -> error "invalid light source name"
        Just l  -> l

lookupMaybeLightSourceByName :: Text -> Maybe LightSource
lookupMaybeLightSourceByName name = lookup (T.toLower name) lightSourceMap

activateLightSource :: LightSource -> Text -> Double -> IO ()
activateLightSource (Digital pin handles) _ _ = setPinLevel handles pin High
activateLightSource source filterName power = undefined

deactivateLightSource :: LightSource -> IO ()
deactivateLightSource (Digital pin handles) = setPinLevel handles pin Low
deactivateLightSource source = undefined

deactivateAllLightSources :: IO ()
deactivateAllLightSources =
    mapM_ closeLightSource (filter lightSourceIsOpen allLightSources)

openLightSource :: LightSource -> IO ()
openLightSource (Digital _ _) = return ()
openLightSource source = undefined

closeLightSource :: LightSource -> IO ()
closeLightSource (Digital _ _) = return ()
closeLightSource source = undefined

lightSourceIsOpen :: LightSource -> Bool
lightSourceIsOpen (Digital _ _) = True
lightSourceIsOpen source = undefined
