{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Maybe
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import GPIO
import OOSeaBreeze
import SimpleJSONServer

import CuvettorTypes

availablePins :: [GPIOPin]
availablePins = [Pin2]

handlerTimeout :: Int
handlerTimeout = 2 * 1000000

serverSettings = defaultSettings {ssDebugMessages = True}

main :: IO ()
main =
    withGPIOPins (zip availablePins (repeat $ Output Low)) (\gpioPins ->
    putStrLn ("opened GPIO pins: " ++ concat (map show availablePins)) >>
    
    withSeaBreeze (
    bracket fetchAvailableSpectrometer closeAvailableSpectrometer $ \maybeSpectrometer ->
    
    putStrLn (if (isJust maybeSpectrometer) then "opened spectrometer" else "no spectrometer found") >>

    putStrLn ("running server") >>
    return (Environment gpioPins availablePins maybeSpectrometer) >>= \env ->
    runServer 3200 messageHandler env serverSettings
    ))

messageHandler :: MessageHandler Environment
messageHandler msg env =
    case (fromJSON msg) of
        Error _   -> return (ResponseJSON (object [("responsetype", "invalidquery")]), env)
        Success v -> performAction env v >>= \(resp, newEnv) -> return (ResponseLBS (encode resp), newEnv)

performAction :: Environment -> RequestMessage -> IO (ResponseMessage, Environment)
performAction env (SetPinHigh pin) = setPinLevelOrError env pin High
performAction env (SetPinLow pin)  = setPinLevelOrError env pin Low

performAction env (AcquireSpectrum exposure nSpectra) =
    --ifSpectrometer maybeSpectrometer (\ids -> acquireSpectrum ids exposure nSpectra) >>= \spectrum ->
    let spectrum = Right dummySpectrum in
    case spectrum of
        Left err -> return (StatusError err, env)
        Right v  -> return (AcquiredSpectrum v, env)
    where
        maybeSpectrometer = envSpectrometer env

performAction env SendWavelengths =
    ifSpectrometer maybeSpectrometer acquireWavelengths >>= \wavelengths ->
    case wavelengths of
        Left err -> return (StatusError err, env)
        Right v  -> return (Wavelengths v, env)
    where
        maybeSpectrometer = envSpectrometer env

dummySpectrum = V.fromList [0.0 .. 3599.0]

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
    if ((exposure <= 0.0) || (nSpectra < 1))
        then return (Left "invalid number of spectra or exposure time")
        else
          runExceptT (
              ExceptT (setIntegrationTimeMicros deviceID featureID integrationMicroseconds) >>
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
