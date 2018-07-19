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

import AcquiredDataTypes
import CuvettorTypes
import Detector
import AvailableDetector
import Equipment
import EquipmentTypes
import EquipmentInitialization
import SimpleJSONServer
import MeasurementProgram
import MeasurementProgramTypes
import MeasurementProgramUtils
import MeasurementProgramVerification
import MiscUtils
import BinaryEncoding

handlerTimeout :: Int
handlerTimeout = 2 * 1000000

serverSettings = defaultSettings {ssBindToAllInterfaces = False,
                                  ssHandlerTimeout = Just (round 10e6),
                                  ssReceiveTimeout = Just (round 2e6),
                                  ssMaxMessageSize = round 2e6}

main :: IO ()
main =
    getExecutablePath >>= \exePath ->
    readAvailableEquipment >>= \descs ->
    withEquipment descs $ \availableEquipment ->
      read <$> readFile (takeDirectory exePath </> "cameraoptions.txt") >>= \imageOrientationOps ->

      newMVar [] >>= \asyncSpectraMVar ->
      newMVar [] >>= \asyncStatusMessagesMVar ->
      async (return ()) >>= \asyncProgramWorker ->
      wait asyncProgramWorker >>

      withAvailableDetector (\det ->
          setImageOrientation det imageOrientationOps >>
          getDetectorWavelengths det >>= \wl ->
          return (byteStringFromVector wl) >>= \encodedWl ->
          putStrLn "ready to measure!" >>
          putStrLn "HOLD CONTROL-C UNTIL YOU SEE \"USER INTERRUPT\" BEFORE CLOSING THIS WINDOW" >>
          let env = Environment availableEquipment det encodedWl
                                asyncSpectraMVar asyncStatusMessagesMVar asyncProgramWorker
          in wait =<< async (runServer 3200 messageHandler env serverSettings))

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
    let detElem = MEDetection ["Default"]
        ddets = M.fromList [("Default", params)]
    in  startAsyncAcquisition env ddets detElem >>= \(asyncWorker, spectraMVar, statusMVar) ->
        wait asyncWorker >>
        takeMVar spectraMVar >>= \[acquiredData] ->
        return (AcquiredDataResponse acquiredData, env)

performAction env ListWavelengths =
    getTime Monotonic >>= \timeStamp ->
    return (Wavelengths (AcquiredData nWavelengths 1 timeStamp wavelengths numType), env)
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

performAction env GetDetectorProperties =
    ensureAsyncAcquisitionNotRunning env >>
    getDetectorOptions det >>= \params ->
    getDetectorFrameRate det >>= \fr ->
    return (DetectorPropertiesResponse params fr, env)
    where
        det = envDetector env

performAction env (SetDetectorProperty prop) =
    ensureAsyncAcquisitionNotRunning env >>
    setDetectorOption det prop >>
    return (StatusOK, env)
    where
        det = envDetector env

performAction env (ActivateLightSource eqName name channels powers) =
    ensureAsyncAcquisitionNotRunning env >>
    return (lookupLightSource eqs (eqName, name)) >>= \eq ->
    activateLightSource eq name (zip channels powers) >>
    return (StatusOK, env)
    where
        eqs = envEquipment env

performAction env (DeactivateLightSource eqName) =
    ensureAsyncAcquisitionNotRunning env >>
    deactivateLightSource eq >>
    return (StatusOK, env)
    where
        [eq]  = filter (\e -> equipmentName e == eqName) (envEquipment env)

performAction env (TurnOffLightSource name) =
    putStrLn "turning off light sources not supported" >>
    return (StatusOK, env)

performAction env Ping = return (Pong, env)

performAction env (ExecuteMeasurementProgram me ddets) =
    startAsyncAcquisition env ddets me >>= \(asyncWorker, spectraMVar, statusMVar) ->
    let newEnv = env {envAsyncDataMVar = spectraMVar, envAsyncStatusMessagesMVar = statusMVar, envAsyncProgramWorker = asyncWorker}
    in  return (StatusOK, newEnv)

performAction env FetchAsyncData =
    asyncAcquisitionRunning env >>= \asyncIsRunning ->
    readMVar dataMVar >>= \newData ->
    asyncAcquisitionErrorMessage env >>= \asyncErrorMsg ->
    return (dataResponse asyncErrorMsg asyncIsRunning (reverse newData), env)
    where
        dataMVar = envAsyncDataMVar env
        wl = envEncodedSpectrometerWavelengths env
        dataResponse asyncErrorMsg asyncIsRunning newData
            | not (null asyncErrorMsg) = StatusError asyncErrorMsg
            | asyncIsRunning           = if (null newData) then StatusNoNewAsyncData else (AsyncAcquiredData newData)
            | otherwise                = if (null newData) then StatusNoNewAsyncDataComing else (AsyncAcquiredData newData)

performAction env (AcknowledgeDataReceipt upToIdx) =
    modifyMVar_ dataMVar (\ds -> pure (filter (\d -> (amdSequence . fst) d > upToIdx) ds)) >>
    return (StatusOK, env)
    where
        dataMVar = envAsyncDataMVar env

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

startAsyncAcquisition :: Detector a => Environment a -> DefinedDetections -> MeasurementElement -> IO (Async (), MVar [(AcquisitionMetaData, AcquiredData)], MVar [Text])
startAsyncAcquisition env ddets me =
    ensureAsyncAcquisitionNotRunning env >>
    validateMeasurementElementThrows (envEquipment env) me ddets >>
    newMVar [] >>= \spectraMVar ->
    newMVar [] >>= \statusMVar ->
    newIORef 0 >>= \dataCounter ->
    getTime Monotonic >>= \startTime ->
    async (executeMeasurement (ProgramEnvironment detector startTime (envEquipment env) dataCounter spectraMVar statusMVar) me ddets >>
           return ()) >>= \asyncWorker ->
    return (asyncWorker, spectraMVar, statusMVar)
    where
        detector = envDetector env

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
