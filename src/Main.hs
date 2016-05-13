{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
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
import IrradiationProgram

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
    nonlinearityCorrection maybeSpectrometer >>= \nonlinearityCorrFunc ->
    
    putStrLn (if (isJust maybeSpectrometer) then "opened spectrometer" else "no spectrometer found") >>
    
    putStrLn ("running server") >>
    newEmptyMVar >>= \asyncSpectraMVar ->
    async (return ()) >>= \asyncProgramWorker ->
    wait asyncProgramWorker >>
    let env = Environment gpioPins availablePins maybeSpectrometer nonlinearityCorrFunc encodedWavelengths asyncSpectraMVar asyncProgramWorker
    in runServer 3200 messageHandler env serverSettings
    ))
    where
        fetchEncodedWavelengths :: Maybe (DeviceID, FeatureID) -> IO Text
        fetchEncodedWavelengths Nothing = return T.empty
        fetchEncodedWavelengths (Just (dID, fID)) =
            getWavelengths dID fID >>= \(Right wLengths) ->
            return . T.decodeUtf8 . B64.encode . byteStringFromVector $ wLengths
        nonlinearityCorrection :: Maybe (DeviceID, FeatureID) -> IO (Double -> Double)
        nonlinearityCorrection Nothing = return id
        nonlinearityCorrection (Just (dID, _)) = 
            V.toList <$> getNonlinearityCoeffs dID >>= \coeffs ->
            return (\x -> x / polyCorr x coeffs)
            where
                polyCorr x coeffs = sum $ zipWith3 (\x coeff order -> coeff * x^order) (repeat x) coeffs ([0 ..] :: [Int])

messageHandler :: MessageHandler Environment
messageHandler msg env =
    case (fromJSON msg) of
        Error _   -> return (ResponseJSON (object [("responsetype", "invalidquery")]), env)
        Success v -> performAction env v >>= \(resp, newEnv) -> return (ResponseLBS (encode resp), newEnv)

performAction :: Environment -> RequestMessage -> IO (ResponseMessage, Environment)
performAction env (SetPinHigh pin) = setPinLevelOrError env pin High
performAction env (SetPinLow pin)  = setPinLevelOrError env pin Low

performAction env (AcquireSpectrum exposure nSpectra) =
    runExceptT (
        ExceptT (return $ ensureSpectrometerAvailable maybeSpectrometer) >>
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (acquireSpectrum (fromJust maybeSpectrometer) exposure nSpectra)) >>= \spectrum ->
    case spectrum of
        Left err -> return (StatusError err, env)
        Right v  -> return (AcquiredSpectrum (V.map nonlinearityCorrection v) wl, env)
    where
        maybeSpectrometer = envSpectrometer env
        wl = envEncodedSpectrometerWavelengths env
        nonlinearityCorrection = envSpectrometerNonlinearityCorrection env

performAction env SendWavelengths =
    runExceptT (
        ExceptT (return $ ensureSpectrometerAvailable maybeSpectrometer) >>
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (acquireWavelengths (fromJust maybeSpectrometer))) >>= \wavelengths ->
    case wavelengths of
        Left err -> return (StatusError err, env)
        Right v  -> return (Wavelengths v, env)
    where
        maybeSpectrometer = envSpectrometer env

performAction env Ping = return (Pong, env)

performAction env (ExecuteIrradiationProgram prog) =
    runExceptT (
        ExceptT (return $ ensureSpectrometerAvailable maybeSpectrometer) >>
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (return $ validateIrradiationProgram prog)) >>= \validation ->
    case validation of
        Left err -> return (StatusError err, env)
        Right _  -> newEmptyMVar >>= \spectraMVar ->
                    async (executeIrradiationProgram prog (ProgramEnvironment (fromJust maybeSpectrometer) spectraMVar)) >>= \asyncWorker ->
                    let newEnv = env {envAsyncSpectraMVar = spectraMVar, envAsyncProgramWorker = asyncWorker}
                    in return (StatusOK, newEnv)
    where
        maybeSpectrometer = envSpectrometer env

performAction env FetchAsyncSpectra =
    modifyMVar spectraMVar (\s -> return ([], s)) >>= \newSpectra ->
    asyncAcquisitionErrorMessage env >>= \asyncErrorMsg ->
    asyncAcquisitionRunning env >>= \asyncIsRunning ->
    return (specResponse asyncErrorMsg asyncIsRunning newSpectra, env)
    where
        spectraMVar = envAsyncSpectraMVar env
        wl = envEncodedSpectrometerWavelengths env
        specResponse asyncErrorMsg asyncIsRunning newSpectra
            | not (null asyncErrorMsg) = StatusError asyncErrorMsg
            | asyncIsRunning           = if (null newSpectra) then StatusNoNewAsyncSpectra else AsyncAcquiredSpectra newSpectra wl
            | otherwise                = if (null newSpectra) then (StatusError "no async acquisition running") else (AsyncAcquiredSpectra newSpectra wl)

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

setPinLevelOrError :: Environment -> GPIOPin -> Level -> IO (ResponseMessage, Environment)
setPinLevelOrError env pin level =
    if (not havePin)
      then return (StatusError "pin not available", env)
      else
          setPinLevel gpioHandles pin level >> return (StatusOK, env)
    where
        havePin = pin `elem` (envAvailablePins env)
        gpioHandles = envGPIOHandles env

ensureSpectrometerAvailable :: Maybe (DeviceID, FeatureID) -> Either String ()
ensureSpectrometerAvailable Nothing = Left "no spectrometer available"
ensureSpectrometerAvailable (Just _) = Right ()

ensureAsyncAcquisitionNotRunning :: Environment -> IO (Either String ())
ensureAsyncAcquisitionNotRunning env =
    asyncAcquisitionRunning env >>= \isRunning ->
    if isRunning
        then return (Left "async acquisition running")
        else return (Right ())

asyncAcquisitionRunning :: Environment -> IO Bool
asyncAcquisitionRunning env =
    poll worker >>= \workerStatus ->
    case workerStatus of
        Nothing -> return True
        Just _  -> return False
    where
        worker = envAsyncProgramWorker env

asyncAcquisitionErrorMessage :: Environment -> IO String
asyncAcquisitionErrorMessage env =
    poll worker >>= \status ->
    case status of
        Nothing        -> return ""
        Just (Right _) -> return ""
        Just (Left e) -> return $ displayException e
    where
        worker = envAsyncProgramWorker env

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
