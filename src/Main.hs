{-# LANGUAGE OverloadedStrings #-}

module Main where

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

import Detector
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
import GPIO
import SimpleJSONServer
import IrradiationProgram
import LightSources
import MiscUtils

import CuvettorTypes

extraPins :: [GPIOPin]
extraPins = []

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
    nonlinearityCorrection maybeSpectrometer >>= \nonlinearityCorrFunc ->
    putStrLn "opened spectrometer" >>
    let (dID, fID) = fromJust maybeSpectrometer
        detector = OODetector dID fID nonlinearityCorrFunc
    in
#endif
#if WITH_SCCAMERA
    (bracket initializeCameraDLL (\_ -> shutdownCameraDLL) $ \initStatus ->
    when (isLeft initStatus) (error (fromLeft initStatus)) >>
    listConnectedCameras >>= \camNames ->
    when (null camNames) (error "no cameras found") >>
    let camName = head camNames
        encodedWavelengths = T.empty
        detector = SCCamDetector camName
    in
#endif
      let env = Environment availableLightSources lightSources gpioHandles
                extraPins detector encodedWavelengths asyncSpectraMVar asyncProgramWorker
      in runServer 3200 messageHandler env serverSettings
    )))
    where
#ifdef WITH_OCEANOPTICS
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
#endif

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
        Right dat  -> return (AcquiredSpectrum dat wl, env)
    where
        detector = envDetector env
        wl = envEncodedSpectrometerWavelengths env
        lightsources = envLightSources env

performAction env ListLightSources = return (AvailableLightSources availableLightSourceDescs, env)
    where
      availableLightSourceDescs = envLightSourceDescs env

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
