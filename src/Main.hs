{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (
    main
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString as SB
import Data.Either
import Data.IORef
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word
import System.Clock
import System.Environment
import System.FilePath

import Camera.SCCameraTypes
import CuvettorTypes
import Detectors.Detector
import Equipment.Equipment
import Equipment.EquipmentTypes
import Equipment.EquipmentInitialization
import SimpleJSONServer
import Measurements.MeasurementProgram
import Measurements.MeasurementProgramTypes
import Utils.MeasurementProgramUtils
import Measurements.MeasurementProgramVerification
import Utils.MiscUtils

import Debug.Trace as DT

handlerTimeout :: Int
handlerTimeout = 2 * 1000000

serverSettings = defaultSettings {ssBindToAllInterfaces = False,
                                  ssHandlerTimeout = Just (round 30e6),
                                  ssReceiveTimeout = Just (round 2e6),
                                  ssMaxMessageSize = round 2e6}

main :: IO ()
main =
    getExecutablePath >>= \exePath ->
    DT.trace (show exePath)  readAvailableEquipment >>= \descs ->
    withEquipmentAndPluginCameras descs $ \(availableEquipment, availablePluginCams) ->
      
      newMessageChannel >>= \asyncMessageChannel ->
      newMVar [] >>= \asyncStatusMessagesMVar ->
      async (return ()) >>= \asyncProgramWorker ->
      wait asyncProgramWorker >>

      
      getDetectorWavelengths (head availablePluginCams) >>= \wl ->
      return (byteStringFromVector wl) >>= \encodedWl ->
      putStrLn "ready to measure!" >>
      putStrLn "HOLD CONTROL-C UNTIL YOU SEE \"USER INTERRUPT\" BEFORE CLOSING THIS WINDOW" >>
      let env = Environment availableEquipment availablePluginCams encodedWl
                        asyncMessageChannel asyncStatusMessagesMVar asyncProgramWorker
      in  wait =<< async (runServer 3200 messageHandler env serverSettings)

messageHandler :: Detector a => MessageHandler (Environment a)
messageHandler msg env =
    case (fromJSON msg) of
      Error e   -> return (ResponseJSON (object [("responsetype", String (T.pack ("invalidquery: " ++ e)))]), env)
      Success v -> (performAction env v >>= \(resp, newEnv) ->
                   let response = if (shouldBinaryEncode resp)
                                  then ResponseBSList (binaryEncode resp)
                                  else ResponseLBS (encode resp)
                   in response `deepseq` return (response, newEnv)) `catch` (\(e :: IOException) -> putStrLn (displayException e) >>
                                                                                                    return (ResponseLBS (encode (StatusError (displayException e))), env))

performAction :: Detector a => Environment a -> RequestMessage -> IO (ResponseMessage, Environment a)
performAction env (AcquireData params) =
    let detElem = MEDetection [AcquisitionTypeName "Default"] []
        ddets = M.fromList [(AcquisitionTypeName "Default", params)]
    in  startAsyncAcquisition env ddets detElem >>= \(asyncWorker, messageChannel, statusMVar) ->
        wait asyncWorker >>
        readChannelMessages messageChannel >>= \acquiredData ->
        return (AcquiredDataResponse acquiredData, env)

performAction env ListWavelengths =
    return (Wavelengths (AcquiredData nWavelengths 1 (SecondsSinceStartOfExperiment 0) (DetectorName "") wavelengths numType), env)
    where
        wavelengths = envEncodedSpectrometerWavelengths env
        nWavelengths = SB.length wavelengths `div` 8
        numType = FP64

performAction env ListAvailableEquipment =
    return (AvailableEquipment es, env)
    where
      es = envEquipment env

performAction env (GetMotorizedStagePosition name) =
    getStagePosition stage >>= \pos ->
    return (MotorizedStagePosition pos, env)
    where
        stage = lookupStageThrows (envEquipment env) name

performAction env (SetMotorizedStagePosition name ds) =
    setStagePosition stage ds >> return (StatusOK, env)
    where
        stage = lookupStageThrows (envEquipment env) name

performAction env (ListRobotPrograms name) =
    listRobotPrograms (lookupRobotThrows eqs name) >>= \programs ->
    return (RobotProgramsResponse programs, env)
    where
        eqs = envEquipment env

performAction env ListAvailableDetectors =
    return (AvailableDetectorsResponse (map detectorName dets), env)
    where
        dets = envDetectors env

performAction env (GetDetectorProperties detName) =
    if (not haveDetector)
    then pure (StatusError "unknown detector", env)
    else
        ensureAsyncAcquisitionNotRunning env >>
        getDetectorProperties det >>= \params ->
        getDetectorFrameRate det >>= \fr ->
        return (DetectorPropertiesResponse params fr, env)
    where
        haveDetector = detName `elem` (map detectorName dets)
        [det] = filter ((==) detName . detectorName) dets
        dets = envDetectors env

performAction env (SetDetectorProperty detName prop) =
    if (not haveDetector)
    then pure (StatusError "unknown detector", env)
    else
        ensureAsyncAcquisitionNotRunning env >>
        setDetectorOption det prop >>
        return (StatusOK, env)
    where
        haveDetector = detName `elem` (map detectorName dets)
        [det] = filter ((==) detName . detectorName) dets
        dets = envDetectors env

performAction env Ping = return (Pong, env)

performAction env (ExecuteMeasurementProgram me ddets _) =
    startAsyncAcquisition env ddets me >>= \(asyncWorker, spectraMVar, statusMVar) ->
    let newEnv = env {envAsyncDataChannel = spectraMVar, envAsyncStatusMessagesMVar = statusMVar, envAsyncProgramWorker = asyncWorker}
    in  return (StatusOK, newEnv)

performAction env FetchAsyncData =
    asyncAcquisitionRunning env >>= \asyncIsRunning ->
    readChannelMessages messageChannel >>= \newData ->
    asyncAcquisitionErrorMessage env >>= \asyncErrorMsg ->
    return (dataResponse asyncErrorMsg asyncIsRunning (newData), env)
    where
        messageChannel = envAsyncDataChannel env
        wl = envEncodedSpectrometerWavelengths env
        dataResponse asyncErrorMsg asyncIsRunning newData
            | not (null asyncErrorMsg) = StatusError asyncErrorMsg
            | asyncIsRunning           = if (null newData) then StatusNoNewAsyncData else (AsyncAcquiredData newData)
            | otherwise                = if (null newData) then StatusNoNewAsyncDataComing else (AsyncAcquiredData newData)

performAction env (AcknowledgeDataReceipt upToIdx) =
    -- delete all acquired data up to the index that has been received
    deleteChannelMessagesUpToIndex messageChannel upToIdx >>
    return (StatusOK, env)
    where
        messageChannel = envAsyncDataChannel env

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

startAsyncAcquisition :: Detector a => Environment a -> DefinedDetections -> MeasurementElement -> IO (Async (), MessageChannel, MVar [Text])
startAsyncAcquisition env ddets me =
    ensureAsyncAcquisitionNotRunning env >>
    validateMeasurementElementThrows (envDetectors env) (envEquipment env) me ddets >>
    newIORef (DetectionIndex 0) >>= \detectionIdxRef ->
    newMessageChannel >>= \messageChannel ->
    newMVar [] >>= \statusMVar ->
    newSmartProgramsChannel >>= \smartProgramSendChan ->
    TimeAtStartOfExperiment <$> getTime Monotonic >>= \startTime ->
    async (executeMeasurement (ProgramEnvironment detectors startTime (envEquipment env) detectionIdxRef [] Nothing messageChannel statusMVar smartProgramSendChan) me ddets >>
           return ()) >>= \asyncWorker ->
    return (asyncWorker, messageChannel, statusMVar)
    where
        detectors = envDetectors env

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
