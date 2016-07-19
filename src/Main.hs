{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString as SB
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import Data.Maybe
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Detector
import OODetector
import GPIO
import OOSeaBreeze
import SimpleJSONServer
import IrradiationProgram
import LightSources
import MiscUtils

import CuvettorTypes

availablePins :: [GPIOPin]
availablePins = [Pin2, Pin3, Pin4, Pin17]

handlerTimeout :: Int
handlerTimeout = 2 * 1000000

serverSettings = defaultSettings {ssHandlerTimeout = Just (round 2.5e6)}

main :: IO ()
main =
    withGPIOPins (zip requiredGPIOPins (repeat $ Output Low)) (\gpioHandles ->
    putStrLn ("opened GPIO pins: " ++ concat (map show availablePins)) >>
    
    withLightSources gpioHandles availableLightSources (\lightSources ->
    putStrLn ("opened light sources") >>
    
    withSeaBreeze (
    bracket fetchAvailableSpectrometer closeAvailableSpectrometer $ \maybeSpectrometer ->
    fetchEncodedWavelengths maybeSpectrometer >>= \encodedWavelengths ->
    nonlinearityCorrection maybeSpectrometer >>= \nonlinearityCorrFunc ->
    putStrLn (if (isJust maybeSpectrometer) then "opened spectrometer" else (error "no spectrometer found")) >>
    
    putStrLn ("running server") >>
    newMVar [] >>= \asyncSpectraMVar ->
    async (return ()) >>= \asyncProgramWorker ->
    wait asyncProgramWorker >>
    let (dID, fID) = fromJust maybeSpectrometer
        env = Environment lightSources gpioHandles availablePins (OODetector dID fID nonlinearityCorrFunc) encodedWavelengths asyncSpectraMVar asyncProgramWorker
    in runServer 3200 messageHandler env serverSettings
    )))
    where
        requiredGPIOPins :: [GPIOPin]
        requiredGPIOPins = nub (availablePins ++ (gpioPinsNeededForLightSources availableLightSources))
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

messageHandler :: Detector a => MessageHandler (Environment a)
messageHandler msg env =
    case (fromJSON msg) of
        Error _   -> return (ResponseJSON (object [("responsetype", "invalidquery")]), env)
        Success v -> performAction env v >>= \(resp, newEnv) -> return (ResponseLBS (encode resp), newEnv)

performAction :: Detector a => Environment a -> RequestMessage -> IO (ResponseMessage, Environment a)
performAction env (SetPinHigh pin) = setPinLevelOrError env pin High
performAction env (SetPinLow pin)  = setPinLevelOrError env pin Low

performAction env (AcquireSpectrum params) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (executeDetection detector lightsources params)) >>= \acquiredData ->
    case acquiredData of
        Left err -> return (StatusError err, env)
        Right (AcquiredData _ _ bytes _)  -> return (AcquiredSpectrum bytes wl, env)
    where
        detector = envDetector env
        wl = envEncodedSpectrometerWavelengths env
        lightsources = envLightSources env

performAction env ListLightSources = return (AvailableLightSources availableLightSources, env)

performAction env (ActivateLightSource name channel power) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (return $ lookupEitherLightSource lightSources name) >>= \lightSource ->
        ExceptT (activateLightSource lightSource channel power)) >>= \result ->
        case result of
            Left err -> return (StatusError err, env)
            Right _ -> return (StatusOK, env)
    where
        lightSources = envLightSources env

performAction env (DeactivateLightSource name) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (return $ lookupEitherLightSource lightSources name) >>= \lightSource ->
        ExceptT (deactivateLightSource lightSource)) >>= \result ->
        case result of
            Left err -> return (StatusError err, env)
            Right _ -> return (StatusOK, env)
    where
        lightSources = envLightSources env

performAction env Ping = return (Pong, env)

performAction env (ExecuteIrradiationProgram prog) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (return $ validateIrradiationProgram lightSources prog)) >>= \validation ->
    case validation of
        Left err -> return (StatusError err, env)
        Right _  -> newMVar [] >>= \spectraMVar ->
                    async (executeIrradiationProgram prog (ProgramEnvironment detector lightSources spectraMVar)) >>= \asyncWorker ->
                    let newEnv = env {envAsyncDataMVar = spectraMVar, envAsyncProgramWorker = asyncWorker}
                    in return (StatusOK, newEnv)
    where
        detector = envDetector env
        lightSources = envLightSources env

performAction env FetchAsyncSpectra =
    modifyMVar spectraMVar (\s -> return ([], s)) >>= \newSpectra ->
    asyncAcquisitionErrorMessage env >>= \asyncErrorMsg ->
    asyncAcquisitionRunning env >>= \asyncIsRunning ->
    return (specResponse asyncErrorMsg asyncIsRunning (extractBytes newSpectra), env)
    where
        extractBytes :: [[(AcquiredData, Double)]] -> [[(SB.ByteString, Double)]]
        extractBytes = map (map (\(d, t) -> (acqData d, t)))
        spectraMVar = envAsyncDataMVar env
        wl = envEncodedSpectrometerWavelengths env
        specResponse asyncErrorMsg asyncIsRunning newSpectra
            | not (null asyncErrorMsg) = StatusError asyncErrorMsg
            | asyncIsRunning           = if (null newSpectra) then StatusNoNewAsyncSpectra else AsyncAcquiredSpectra newSpectra wl
            | otherwise                = if (null newSpectra) then StatusNoNewAsyncSpectraComing else (AsyncAcquiredSpectra newSpectra wl)

performAction env CancelAsyncAcquisition =
    asyncAcquisitionRunning env >>= \asyncIsRunning ->
    if (asyncIsRunning)
      then cancel worker >> return (StatusOK, env)
      else return (StatusError "no async acquisition running", env)
    where
        worker = envAsyncProgramWorker env

performAction env IsAsyncAcquisitionRunning =
    asyncAcquisitionRunning env >>= \asyncIsRunning ->
    return (AsyncAcquisitionIsRunning asyncIsRunning, env)

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

setPinLevelOrError :: Environment a -> GPIOPin -> Level -> IO (ResponseMessage, Environment a)
setPinLevelOrError env pin level =
    if (not havePin)
      then return (StatusError "pin not available", env)
      else
          setPinLevel gpioHandles pin level >> return (StatusOK, env)
    where
        havePin = pin `elem` (envAvailablePins env)
        gpioHandles = envGPIOHandles env

ensureAsyncAcquisitionNotRunning :: Environment a -> IO (Either String ())
ensureAsyncAcquisitionNotRunning env =
    asyncAcquisitionRunning env >>= \isRunning ->
    if isRunning
        then return (Left "async acquisition running")
        else return (Right ())

asyncAcquisitionRunning :: Environment a -> IO Bool
asyncAcquisitionRunning env =
    poll worker >>= \workerStatus ->
    case workerStatus of
        Nothing -> return True
        Just _  -> return False
    where
        worker = envAsyncProgramWorker env

asyncAcquisitionErrorMessage :: Environment a -> IO String
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
