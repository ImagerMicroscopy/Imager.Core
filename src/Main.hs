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
import GPIO
import SimpleJSONServer
import IrradiationProgram
import LightSources
import MiscUtils
import BinaryEncoding

#ifdef WITH_OCEANOPTICS
import OOSeaBreeze
import OODetector
#endif
#ifdef WITH_SCCAMERA
import SCCamera
import SCCamDetector
#endif
#if !defined(WITH_OCEANOPTICS) && !defined(WITH_SCCAMERA)
    #error "building without any detector support"
#endif
#if defined(WITH_OCEANOPTICS) && defined(WITH_SCCAMERA)
    #error "must build for either camera or spectrometer"
#endif

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

    withGPIOPins (zip requiredGPIOPins (repeat $ Output Low)) (\gpioHandles ->
    putStrLn ("opened GPIO pins: " ++ concat (map show requiredGPIOPins)) >>

    withLightSources gpioHandles availableLightSources (\lightSources ->
    putStrLn ("opened light sources") >>

    newMVar [] >>= \asyncSpectraMVar ->
    async (return ()) >>= \asyncProgramWorker ->
    wait asyncProgramWorker >>

#ifdef WITH_OCEANOPTICS
    (withSeaBreeze $
    bracket fetchAvailableSpectrometer closeAvailableSpectrometer $ \maybeSpectrometer ->
    fetchEncodedWavelengths maybeSpectrometer >>= \encodedWavelengths ->
    setTriggerMode maybeSpectrometer >>
    nonlinearityCorrection maybeSpectrometer >>= \nonlinearityCorrFunc ->
    putStrLn "opened spectrometer" >>
    let (dID, fID) = fromJust maybeSpectrometer
        detector = OODetector dID fID (Just (spectrometerTriggerPin, gpioHandles)) nonlinearityCorrFunc
    in
#endif
#if WITH_SCCAMERA
    (bracket initializeCameraDLL (\_ -> shutdownCameraDLL) $ \initStatus ->
    when (isLeft initStatus) (error (fromLeft initStatus)) >>
    listConnectedCameras >>= \camNames ->
    when (null camNames) (error "no cameras found") >>
    let camName = head camNames
        encodedWavelengths = SB.empty
        detector = SCCamDetector camName
    in
#endif
      let env = Environment lightSources gpioHandles
                extraPins detector encodedWavelengths asyncSpectraMVar asyncProgramWorker
      in runServer 3200 messageHandler env serverSettings
    )))
    where
#ifdef WITH_OCEANOPTICS
        fetchEncodedWavelengths :: Maybe (DeviceID, FeatureID) -> IO SB.ByteString
        fetchEncodedWavelengths Nothing = return SB.empty
        fetchEncodedWavelengths (Just (dID, fID)) =
            getWavelengths dID fID >>= \(Right wLengths) ->
            return (byteStringFromVector wLengths)
        setTriggerMode :: Maybe (dID, fID) -> IO ()
        setTriggerMode Nothing = return ()
        setTriggerMode (Just (dID, fID)) = return () --setTriggerMode fID dID TriggerExternalHardwareLevel
        nonlinearityCorrection :: Maybe (DeviceID, FeatureID) -> IO (Double -> Double)
        nonlinearityCorrection Nothing = return id
        nonlinearityCorrection (Just (dID, _)) =
            V.toList <$> getNonlinearityCoeffs dID >>= \coeffs ->
            return (\x -> x / polyCorr x coeffs)
            where
                polyCorr x coeffs = sum $ zipWith3 (\x coeff order -> coeff * x^order) (repeat x) coeffs ([0 ..] :: [Int])
#endif

messageHandler :: Detector a => MessageHandler (Environment a)
messageHandler msg env =
    case (fromJSON msg) of
      Error _   -> return (ResponseJSON (object [("responsetype", "invalidquery")]), env)
      Success v -> try (performAction env v) >>= \result ->
        case result of
          Left (exc :: SomeException) -> putStrLn (displayException exc) >> throwIO exc
          Right (resp, newEnv) ->
            if (shouldBinaryEncode resp)
            then return (ResponseBSList (binaryEncode resp), newEnv)
            else return (ResponseLBS (encode resp), newEnv)

performAction :: Detector a => Environment a -> RequestMessage -> IO (ResponseMessage, Environment a)
performAction env (SetPinHigh pin) = setPinLevelOrError env pin High
performAction env (SetPinLow pin)  = setPinLevelOrError env pin Low

performAction env (AcquireData params) =
    runExceptT (
        ExceptT (ensureAsyncAcquisitionNotRunning env) >>
        ExceptT (executeDetection detector lightsources params)) >>= \acquiredData ->
    case acquiredData of
        Left err -> return (StatusError err, env)
        Right dat  -> return (AcquiredDataResponse dat, env)
    where
        detector = envDetector env
        lightsources = envLightSources env

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
