{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString as SB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import Data.Maybe
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import GPIO
import OOSeaBreeze
import SimpleJSONServer

import CuvettorTypes

availablePins :: [GPIOPin]
availablePins = [Pin2, Pin3, Pin4, Pin17]

handlerTimeout :: Int
handlerTimeout = 2 * 1000000

serverSettings = defaultSettings {ssHandlerTimeout = Just (round 2.5e6)}

main :: IO ()
main =
    withGPIOPins (zip availablePins (repeat $ Output Low)) (\gpioPins ->
    putStrLn ("opened GPIO pins: " ++ concat (map show availablePins)) >>
    
    withSeaBreeze (
    bracket fetchAvailableSpectrometer closeAvailableSpectrometer $ \maybeSpectrometer ->
    fetchEncodedWavelengths maybeSpectrometer >>= \encodedWavelengths ->
    
    putStrLn (if (isJust maybeSpectrometer) then "opened spectrometer" else "no spectrometer found") >>

    putStrLn ("running server") >>
    return (Environment gpioPins availablePins maybeSpectrometer encodedWavelengths) >>= \env ->
    runServer 3200 messageHandler env serverSettings
    ))
    where
        fetchEncodedWavelengths :: Maybe (DeviceID, FeatureID) -> IO Text
        fetchEncodedWavelengths Nothing = return T.empty
        fetchEncodedWavelengths (Just (dID, fID)) =
            getWavelengths dID fID >>= \(Right wLengths) ->
            return . T.decodeUtf8 . B64.encode . byteStringFromVector $ wLengths

messageHandler :: MessageHandler Environment
messageHandler msg env =
    case (fromJSON msg) of
        Error _   -> return (ResponseJSON (object [("responsetype", "invalidquery")]), env)
        Success v -> performAction env v >>= \(resp, newEnv) -> return (ResponseLBS (encode resp), newEnv)

performAction :: Environment -> RequestMessage -> IO (ResponseMessage, Environment)
performAction env (SetPinHigh pin) = setPinLevelOrError env pin High
performAction env (SetPinLow pin)  = setPinLevelOrError env pin Low

performAction env (AcquireSpectrum exposure nSpectra) =
    ifSpectrometer maybeSpectrometer (\ids -> acquireSpectrum ids exposure nSpectra) >>= \spectrum ->
    case spectrum of
        Left err -> return (StatusError err, env)
        Right v  -> return (AcquiredSpectrum v wl, env)
    where
        maybeSpectrometer = envSpectrometer env
        wl = envEncodedSpectrometerWavelengths env

performAction env SendWavelengths =
    ifSpectrometer maybeSpectrometer acquireWavelengths >>= \wavelengths ->
    case wavelengths of
        Left err -> return (StatusError err, env)
        Right v  -> return (Wavelengths v, env)
    where
        maybeSpectrometer = envSpectrometer env

performAction env Ping = return (Pong, env)

setPinLevelOrError :: Environment -> GPIOPin -> Level -> IO (ResponseMessage, Environment)
setPinLevelOrError env pin level =
    if (not havePin)
      then return (StatusError "pin not available", env)
      else
          setPinLevel gpioHandles pin level >> return (StatusOK, env)
    where
        havePin = pin `elem` (envAvailablePins env)
        gpioHandles = envGPIOHandles env

ifSpectrometer :: Maybe (DeviceID, FeatureID) -> ((DeviceID, FeatureID) -> IO (Either String (Vector Double))) -> IO (Either String (Vector Double))
ifSpectrometer Nothing _ = return $ Left "no spectrometer available"
ifSpectrometer (Just ids) action = action ids

acquireSpectrum :: (DeviceID, FeatureID) -> Double -> Int -> IO (Either String (Vector Double))
acquireSpectrum (deviceID, featureID) exposure nSpectra =
    if ((exposure <= 0.0) || (exposure > 1.0) || (nSpectra < 1))
        then return (Left "invalid number of spectra or exposure time")
        else
          runExceptT (
              ExceptT (setIntegrationTimeMicros deviceID featureID integrationMicroseconds) >>
              ExceptT (measureSpectrum deviceID featureID) >>   -- force a fresh acquisition
              ExceptT (measureAveragedSpectrum deviceID featureID nSpectra))
    where
        integrationMicroseconds = floor (exposure * 1e6)

acquireWavelengths :: (DeviceID, FeatureID) -> IO (Either String (Vector Double))
acquireWavelengths (deviceID, featureID) =
    getWavelengths deviceID featureID

fetchAvailableSpectrometer :: IO (Maybe (DeviceID, FeatureID))
fetchAvailableSpectrometer =
    getDeviceIDs >>= \idList ->
    if (not $ null idList)
      then
        runExceptT (
            ExceptT (openDevice (head idList)) >>
            ExceptT (getSpectrometerFeatures (head idList)) >>= \(featureID : _) ->
            return (head idList, featureID)) >>= \result ->
        case result of
            Left _  -> return Nothing
            Right v -> return $ Just v
      else return Nothing

closeAvailableSpectrometer :: Maybe (DeviceID, FeatureID) -> IO ()
closeAvailableSpectrometer Nothing              = return ()
closeAvailableSpectrometer (Just (deviceID, _)) = closeDevice deviceID >> return ()
