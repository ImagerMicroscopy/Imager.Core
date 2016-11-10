{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (
    main
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString as SB
import Data.Either
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import Data.Maybe
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import System.Clock

import CuvettorTypes
import Detector
import AvailableDetector
import GPIO
import SimpleJSONServer
import IrradiationProgram
import LightSources
import MotorizedStage
import MiscUtils
import BinaryEncoding
import FilterWheel

extraPins :: [GPIOPin]
#ifdef WITH_OCEANOPTICS
extraPins = [spectrometerTriggerPin]
spectrometerTriggerPin = Pin11
#else
extraPins = []
#endif

handlerTimeout :: Int
handlerTimeout = 2 * 1000000

serverSettings = defaultSettings {ssHandlerTimeout = Just (round 2.5e6)}

main :: IO ()
main =
    readAvailableLightSources >>= \availableLightSources ->
    return (nub (extraPins ++ (gpioPinsNeededForLightSources availableLightSources))) >>= \requiredGPIOPins ->

    readAvailableFilterWheels >>= \availableFilterWheels ->
    readAvailableMotorizedStages >>= \availableStages ->

    withGPIOPins (zip requiredGPIOPins (repeat $ Output Low)) (\gpioHandles ->
    putStrLn ("opened GPIO pins: " ++ concat (map show requiredGPIOPins)) >>

    withLightSources gpioHandles availableLightSources (\lightSources ->
    putStrLn ("opened light sources") >>

    withFilterWheels availableFilterWheels (\filterWheels ->
    putStrLn ("opened filter wheels") >>

    withMotorizedStages availableStages (\motorizedStages ->
    putStrLn ("opened motorized stages") >>

    newMVar [] >>= \asyncSpectraMVar ->
    async (return ()) >>= \asyncProgramWorker ->
    wait asyncProgramWorker >>

    withAvailableDetector (\det ->
        getDetectorWavelengths det >>= \(Right wl) ->
        return (byteStringFromVector wl) >>= \encodedWl ->
        let env = Environment lightSources filterWheels motorizedStages gpioHandles
              extraPins det encodedWl asyncSpectraMVar asyncProgramWorker
        in runServer 3200 messageHandler env serverSettings)))))

messageHandler :: Detector a => MessageHandler (Environment a)
messageHandler msg env =
    case (fromJSON msg) of
      Error _   -> return (ResponseJSON (object [("responsetype", "invalidquery")]), env)
      Success v -> performAction env v >>= \(resp, newEnv) ->
                   if (shouldBinaryEncode resp)
                     then return (ResponseBSList (binaryEncode resp), newEnv)
                     else return (ResponseLBS (encode resp), newEnv)

performAction :: Detector a => Environment a -> RequestMessage -> IO (ResponseMessage, Environment a)
performAction env (SetPinHigh pin) = setPinLevelOrError env pin High
performAction env (SetPinLow pin)  = setPinLevelOrError env pin Low

performAction env (AcquireData params) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (return $ validateDetectionParams lightsources filterWheels params) >>
        ExceptT (executeDetection detector lightsources filterWheels params)) >>= \acquiredData ->
    case acquiredData of
        Left err -> return (StatusError err, env)
        Right dat  -> return (AcquiredDataResponse dat, env)
    where
        detector = envDetector env
        lightsources = envLightSources env
        filterWheels = envFilterWheels env

performAction env ListWavelengths =
    getTime Monotonic >>= \timeStamp ->
    return (Wavelengths (AcquiredData nWavelengths 1 timeStamp wavelengths numType), env)
    where
        wavelengths = envEncodedSpectrometerWavelengths env
        nWavelengths = SB.length wavelengths `div` 8
        numType = FP64

performAction env ListLightSources = return (AvailableLightSources lss, env)
    where
      lss = envLightSources env

performAction env ListFilterWheels = return (AvailableFilterWheels fws, env)
    where
        fws = envFilterWheels env

performAction env ListMotorizedStages = return (AvailableMotorizedStages mss, env)
    where
        mss = envMotorizedStages env

performAction env (GetMotorizedStagePosition name) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (getStagePositionLookup mss name)) >>= \result ->
    case result of
        Left err -> return (StatusError err, env)
        Right ds -> return (MotorizedStagePosition ds, env)
    where
        mss = envMotorizedStages env

performAction env (SetMotorizedStagePosition name ds) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (setStagePositionLookup mss name ds)) >>= \result ->
    case result of
        Left err -> return (StatusError err, env)
        Right ds -> return (StatusOK, env)
    where
        mss = envMotorizedStages env

performAction env GetDetectorLimits =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (getDetectorLimits det)) >>= \limits ->
    case limits of
        Left err -> return (StatusError err, env)
        Right dl -> return (DetectorLimitsResponse dl, env)
    where
        det = envDetector env

performAction env (SetDetectorTemperature t) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (setDetectorTemperature det t)) >>= \result ->
    case result of
        Left err -> return (StatusError err, env)
        Right () -> return (StatusOK, env)
    where
        det = envDetector env

performAction env GetDetectorTemperature =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (getDetectorTemperature det)) >>= \temp ->
    case temp of
        Left err -> return (StatusError err, env)
        Right t -> return (DetectorTemperatureResponse t, env)
    where
        det = envDetector env

performAction env GetDetectorTemperatureSetpoint =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (getDetectorTemperatureSetpoint det)) >>= \temp ->
    case temp of
        Left err -> return (StatusError err, env)
        Right t -> return (DetectorTemperatureSetpointResponse t, env)
    where
        det = envDetector env

performAction env (ActivateLightSource name channels powers) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (return $ lookupEitherLightSource lightSources name) >>= \lightSource ->
        ExceptT (activateLightSource lightSource channels powers)) >>= \result ->
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

performAction env (TurnOffLightSource name) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (return $ lookupEitherLightSource lightSources name) >>= \lightSource ->
        ExceptT (turnOffLightSource lightSource)) >>= \result ->
    case result of
        Left err -> return (StatusError err, env)
        Right _ -> return (StatusOK, env)
    where
        lightSources = envLightSources env

performAction env Ping = return (Pong, env)

performAction env (ExecuteIrradiationProgram prog) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (return $ validateIrradiationProgram lightSources filterWheels motorizedStages prog)) >>= \validation ->
    case validation of
        Left err -> return (StatusError err, env)
        Right _  -> newMVar [] >>= \spectraMVar ->
                    async (executeIrradiationProgram prog (ProgramEnvironment detector lightSources filterWheels motorizedStages spectraMVar)) >>= \asyncWorker ->
                    let newEnv = env {envAsyncDataMVar = spectraMVar, envAsyncProgramWorker = asyncWorker}
                    in return (StatusOK, newEnv)
    where
        detector = envDetector env
        lightSources = envLightSources env
        filterWheels = envFilterWheels env
        motorizedStages = envMotorizedStages env

performAction env FetchAsyncData =
    modifyMVar dataMVar (\s -> return ([], s)) >>= \newData ->
    asyncAcquisitionErrorMessage env >>= \asyncErrorMsg ->
    asyncAcquisitionRunning env >>= \asyncIsRunning ->
    return (dataResponse asyncErrorMsg asyncIsRunning (map reverse newData), env)
    where
        dataMVar = envAsyncDataMVar env
        wl = envEncodedSpectrometerWavelengths env
        dataResponse asyncErrorMsg asyncIsRunning newData
            | not (null asyncErrorMsg) = StatusError asyncErrorMsg
            | asyncIsRunning           = if (null newData) then StatusNoNewAsyncData else (AsyncAcquiredData newData)
            | otherwise                = if (null newData) then StatusNoNewAsyncDataComing else (AsyncAcquiredData newData)

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
