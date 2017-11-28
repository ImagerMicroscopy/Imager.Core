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
import EquipmentTypes
import EquipmentInitialization
import SimpleJSONServer
import MeasurementProgram
import MeasurementProgramTypes
import MeasurementProgramVerification
import LightSources
import Robot
import MotorizedStage
import MiscUtils
import BinaryEncoding
import FilterWheel

handlerTimeout :: Int
handlerTimeout = 2 * 1000000

serverSettings = defaultSettings {ssBindToAllInterfaces = False,
                                  ssHandlerTimeout = Just (round 10e6),
                                  ssReceiveTimeout = Just (round 2e6),
                                  ssMaxMessageSize = round 2e6}

main :: IO ()
main = wait =<< async (
    readAvailableEquipment >>= \descs ->
    withEquipment descs $ \availableEquipment ->

    newMVar [] >>= \asyncSpectraMVar ->
    newMVar [] >>= \asyncStatusMessagesMVar ->
    async (return ()) >>= \asyncProgramWorker ->
    wait asyncProgramWorker >>

    withAvailableDetector (\det ->
        getDetectorWavelengths det >>= \wl ->
        return (byteStringFromVector wl) >>= \encodedWl ->
        putStrLn "ready to measure!" >>
        let lightSources = filter isLightSource availableEquipment
            filterWheels = filter isFilterWheel availableEquipment
            motorizedStages = filter isMotorizedStage availableEquipment
            robots = filter isRobot availableEquipment
            env = Environment lightSources filterWheels motorizedStages robots
                      det encodedWl asyncSpectraMVar asyncStatusMessagesMVar asyncProgramWorker
        in runServer 3200 messageHandler env serverSettings))

messageHandler :: Detector a => MessageHandler (Environment a)
messageHandler msg env =
    case (fromJSON msg) of
      Error e   -> return (ResponseJSON (object [("responsetype", String (T.pack ("invalidquery: " ++ e)))]), env)
      Success v -> perform env v >>= \(resp, newEnv) ->
                   if (shouldBinaryEncode resp)
                     then return (ResponseBSList (binaryEncode resp), newEnv)
                     else return (ResponseLBS (encode resp), newEnv)
    where
       perform env v = (performAction env v) `catch` (\(e :: IOException) -> putStrLn (displayException e) >>
                                                                             return (StatusError (displayException e), env))

performAction :: Detector a => Environment a -> RequestMessage -> IO (ResponseMessage, Environment a)
performAction env (AcquireData params) =
    startAsyncAcquisition env (MEDetection [params]) >>= \(asyncWorker, spectraMVar, statusMVar) ->
    wait asyncWorker >>
    takeMVar spectraMVar >>= \[[acquiredData]] ->
    return (AcquiredDataResponse acquiredData, env)

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

performAction env ListRobots = return (AvailableRobots robots, env)
    where
        robots = envRobots env

performAction env (GetMotorizedStagePosition name) =
    getStagePosition stage >>= \pos ->
    return (MotorizedStagePosition pos, env)
    where
        stage = lookupStage (envMotorizedStages env) name

performAction env (SetMotorizedStagePosition name ds) =
    setStagePositionLookup mss name ds >> return (StatusOK, env)
    where
        mss = envMotorizedStages env

performAction env (ListRobotPrograms name) =
    listRobotPrograms (lookupRobotThrows robots name) >>= \programs ->
    return (RobotProgramsResponse programs, env)
    where
        robots = envRobots env

performAction env GetDetectorLimits =
    ensureAsyncAcquisitionNotRunning env >>
    getDetectorLimits det >>= \limits ->
    return (DetectorLimitsResponse limits, env)
    where
        det = envDetector env

performAction env (SetDetectorTemperature t) =
    ensureAsyncAcquisitionNotRunning env >>
    setDetectorTemperature det t >>
    return (StatusOK, env)
    where
        det = envDetector env

performAction env GetDetectorTemperature =
    getDetectorTemperature det >>= \t ->
    return (DetectorTemperatureResponse t, env)
    where
        det = envDetector env

performAction env GetDetectorTemperatureSetpoint =
    ensureAsyncAcquisitionNotRunning env >>
    getDetectorTemperatureSetpoint det >>= \t ->
    return (DetectorTemperatureSetpointResponse t, env)
    where
        det = envDetector env

performAction env (ActivateLightSource name channels powers) =
    ensureAsyncAcquisitionNotRunning env >>
    return (lookupLightSource lightSources name) >>= \lightSource ->
    activateLightSource lightSource channels powers >>
    return (StatusOK, env)
    where
        lightSources = envLightSources env

performAction env (DeactivateLightSource name) =
    ensureAsyncAcquisitionNotRunning env >>
    return (lookupLightSource lightSources name) >>=
    deactivateLightSource >>
    return (StatusOK, env)
    where
        lightSources = envLightSources env

performAction env (TurnOffLightSource name) =
    return (lookupLightSource lightSources name) >>=
    turnOffLightSource >>
    return (StatusOK, env)
    where
        lightSources = envLightSources env

performAction env Ping = return (Pong, env)

performAction env (ExecuteMeasurementProgram me) =
    startAsyncAcquisition env me >>= \(asyncWorker, spectraMVar, statusMVar) ->
    let newEnv = env {envAsyncDataMVar = spectraMVar, envAsyncStatusMessagesMVar = statusMVar, envAsyncProgramWorker = asyncWorker}
    in  return (StatusOK, newEnv)

performAction env FetchAsyncData =
    asyncAcquisitionRunning env >>= \asyncIsRunning ->
    modifyMVar dataMVar (\s -> return ([], s)) >>= \newData ->
    asyncAcquisitionErrorMessage env >>= \asyncErrorMsg ->
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
    validateMeasurementElementThrows lightSources filterWheels motorizedStages robots me >>
    newMVar [] >>= \spectraMVar ->
    newMVar [] >>= \statusMVar ->
    getTime Monotonic >>= \startTime ->
    async (executeMeasurement (ProgramEnvironment detector startTime lightSources filterWheels motorizedStages robots spectraMVar statusMVar) me >>
           return ()) >>= \asyncWorker ->
    return (asyncWorker, spectraMVar, statusMVar)
    where
        detector = envDetector env
        lightSources = envLightSources env
        filterWheels = envFilterWheels env
        motorizedStages = envMotorizedStages env
        robots = envRobots env

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
