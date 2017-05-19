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
import MeasurementProgram
import MeasurementProgramTypes
import MeasurementProgramVerification
import LightSources
import MicroscopeRobot
import MotorizedStage
import MiscUtils
import BinaryEncoding
import FilterWheel

extraPins :: [GPIOPin]
-- #ifdef WITH_OCEANOPTICS
--extraPins = [spectrometerTriggerPin]
--spectrometerTriggerPin = Pin11
-- #else
extraPins = []
-- #endif

handlerTimeout :: Int
handlerTimeout = 2 * 1000000

serverSettings = defaultSettings {ssBindToAllInterfaces = False,
                                  ssHandlerTimeout = Just (round 10e6),
                                  ssReceiveTimeout = Just (round 2e6),
                                  ssMaxMessageSize = round 2e6}

main :: IO ()
main =
    readAvailableLightSources >>= \availableLightSources ->
    return (nub (extraPins ++ (gpioPinsNeededForLightSources availableLightSources))) >>= \requiredGPIOPins ->

    readAvailableFilterWheels >>= \availableFilterWheels ->
    readAvailableMotorizedStages >>= \availableStages ->
    readAvailableMicroscopeRobots >>= \availableRobots ->

    withGPIOPins (zip requiredGPIOPins (repeat $ Output Low)) (\gpioHandles ->
    putStrLn ("opened GPIO pins: " ++ concat (map show requiredGPIOPins)) >>

    withLightSources gpioHandles availableLightSources (\lightSources ->
    putStrLn ("opened light sources") >>

    withFilterWheels availableFilterWheels (\filterWheels ->
    putStrLn ("opened filter wheels") >>

    withMotorizedStages availableStages (\motorizedStages ->
    putStrLn ("opened motorized stages") >>

    withMicroscopeRobots availableRobots (\microscopeRobots ->
    putStrLn ("opened microscope robots") >>

    newMVar [] >>= \asyncSpectraMVar ->
    newMVar [] >>= \asyncStatusMessagesMVar ->
    async (return ()) >>= \asyncProgramWorker ->
    wait asyncProgramWorker >>

    withAvailableDetector (\det ->
        getDetectorWavelengths det >>= \(Right wl) ->
        return (byteStringFromVector wl) >>= \encodedWl ->
        putStrLn "ready to measure!" >>
        let env = Environment lightSources filterWheels motorizedStages microscopeRobots gpioHandles
              extraPins det encodedWl asyncSpectraMVar asyncStatusMessagesMVar asyncProgramWorker
        in runServer 3200 messageHandler env serverSettings))))))

messageHandler :: Detector a => MessageHandler (Environment a)
messageHandler msg env =
    case (fromJSON msg) of
      Error e   -> return (ResponseJSON (object [("responsetype", String (T.pack ("invalidquery: " ++ e)))]), env)
      Success v -> performAction env v >>= \(resp, newEnv) ->
                   if (shouldBinaryEncode resp)
                     then return (ResponseBSList (binaryEncode resp), newEnv)
                     else return (ResponseLBS (encode resp), newEnv)

performAction :: Detector a => Environment a -> RequestMessage -> IO (ResponseMessage, Environment a)
performAction env (AcquireData params) =
    catch (startAsyncAcquisition env (MEDetection [params]) >>= \(asyncWorker, spectraMVar, statusMVar) ->
           wait asyncWorker >>
           takeMVar spectraMVar >>= \[[acquiredData]] ->
           return (AcquiredDataResponse acquiredData, env))
          (\e -> return (StatusError (displayException (e :: IOException)), env))

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

performAction env ListMicroscopeRobots = return (AvailableMicroscopeRobots robots, env)
    where
        robots = envMicroscopeRobots env

performAction env (GetMotorizedStagePosition name) =
    getStagePositionLookup mss name >>= \result ->
    case result of
        Left err -> return (StatusError err, env)
        Right ds -> return (MotorizedStagePosition ds, env)
    where
        mss = envMotorizedStages env

performAction env (SetMotorizedStagePosition name ds) =
    catch (setStagePositionLookup mss name ds >>
           return (StatusOK, env))
          (\e -> return (StatusError (displayException (e :: IOException)), env))
    where
        mss = envMotorizedStages env

performAction env (ListMicroscopeRobotPrograms name) =
    catch (listRobotPrograms robots name >>= \programs ->
           return (MicroscopeRobotProgramsResponse programs, env))
          (\e -> return (StatusError (displayException (e :: IOException)), env))
    where
        robots = envMicroscopeRobots env

performAction env (ExecuteMicroscopeRobotProgram name prog) =
    catch (executeRobotProgram robots name prog >>
           return (StatusOK, env))
          (\e -> return (StatusError (displayException (e :: IOException)), env))
    where
        robots = envMicroscopeRobots env

performAction env GetDetectorLimits =
    catch (ensureAsyncAcquisitionNotRunning env >>
           getDetectorLimits det >>= \limits ->
           case limits of
               Left err -> return (StatusError err, env)
               Right dl -> return (DetectorLimitsResponse dl, env))
          (\e -> return (StatusError (displayException (e :: IOException)), env))
    where
        det = envDetector env

performAction env (SetDetectorTemperature t) =
    catch (ensureAsyncAcquisitionNotRunning env >>
           setDetectorTemperature det t >>= \result ->
           case result of
               Left err -> return (StatusError err, env)
               Right () -> return (StatusOK, env))
          (\e -> return (StatusError (displayException (e :: IOException)), env))
    where
        det = envDetector env

performAction env GetDetectorTemperature =
    catch (ensureAsyncAcquisitionNotRunning env >>
           getDetectorTemperature det >>= \temp ->
           case temp of
               Left err -> return (StatusError err, env)
               Right t -> return (DetectorTemperatureResponse t, env))
          (\e -> return (StatusError (displayException (e :: IOException)), env))
    where
        det = envDetector env

performAction env GetDetectorTemperatureSetpoint =
    catch (ensureAsyncAcquisitionNotRunning env >>
           getDetectorTemperatureSetpoint det >>= \temp ->
           case temp of
              Left err -> return (StatusError err, env)
              Right t -> return (DetectorTemperatureSetpointResponse t, env))
          (\e -> return (StatusError (displayException (e :: IOException)), env))
    where
        det = envDetector env

performAction env (ActivateLightSource name channels powers) =
    catch (ensureAsyncAcquisitionNotRunning env >>
           return (lookupLightSource lightSources name) >>= \lightSource ->
           activateLightSource lightSource channels powers >>
           return (StatusOK, env))
          (\e -> return (StatusError (displayException (e :: IOException)), env))
    where
        lightSources = envLightSources env

performAction env (DeactivateLightSource name) =
    catch (ensureAsyncAcquisitionNotRunning env >>
           return (lookupLightSource lightSources name) >>=
           deactivateLightSource >>
           return (StatusOK, env))
          (\e -> return (StatusError (displayException (e :: IOException)), env))
    where
        lightSources = envLightSources env

performAction env (TurnOffLightSource name) =
    catch (ensureAsyncAcquisitionNotRunning env >>
           return (lookupLightSource lightSources name) >>=
           turnOffLightSource >>
           return (StatusOK, env))
          (\e -> return (StatusError (displayException (e :: IOException)), env))
    where
        lightSources = envLightSources env

performAction env Ping = return (Pong, env)

performAction env (ExecuteMeasurementProgram me) =
    catch (startAsyncAcquisition env me >>= \(asyncWorker, spectraMVar, statusMVar) ->
           let newEnv = env {envAsyncDataMVar = spectraMVar, envAsyncStatusMessagesMVar = statusMVar, envAsyncProgramWorker = asyncWorker}
           in return (StatusOK, newEnv))
          (\e -> return (StatusError (displayException (e :: IOException)), env))

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

performAction env FetchAsyncStatusMessages =
    asyncAcquisitionErrorMessage env >>= \asyncErrorMsg ->
    asyncAcquisitionRunning env >>= \asyncIsRunning ->
    withMVar msgMVar (return . id) >>= \ms ->
    return (response asyncErrorMsg asyncIsRunning ms, env)
    where
        response asyncErrorMsg asyncIsRunning msgs
            | not (null asyncErrorMsg) = StatusError asyncErrorMsg
            | not asyncIsRunning = StatusError "no async acquisition running"
            | otherwise = AsyncStatusMessages (reverse msgs)
        msgMVar = envAsyncStatusMessagesMVar env

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

startAsyncAcquisition :: Detector a => Environment a -> MeasurementElement -> IO (Async (), MVar [[AcquiredData]], MVar [Text])
startAsyncAcquisition env me =
    ensureAsyncAcquisitionNotRunning env >>
    validateMeasurementElementThrows lightSources filterWheels motorizedStages me >>
    newMVar [] >>= \spectraMVar ->
    newMVar [] >>= \statusMVar ->
    getTime Monotonic >>= \startTime ->
    async (executeMeasurement (ProgramEnvironment detector startTime lightSources filterWheels motorizedStages spectraMVar statusMVar) me >>
           return ()) >>= \asyncWorker ->
    return (asyncWorker, spectraMVar, statusMVar)
    where
        detector = envDetector env
        lightSources = envLightSources env
        filterWheels = envFilterWheels env
        motorizedStages = envMotorizedStages env

ensureAsyncAcquisitionNotRunning :: Environment a -> IO ()
ensureAsyncAcquisitionNotRunning env =
    asyncAcquisitionRunning env >>= \isRunning ->
    when (isRunning) (throwIO (userError "async acquisition running"))

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
