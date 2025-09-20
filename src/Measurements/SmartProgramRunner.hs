module Measurements.SmartProgramRunner (
    withRunnableSmartPrograms
) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.MessagePack
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Req
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

import Measurements.MeasurementProgramTypes
import Utils.MiscUtils

withRunnableSmartPrograms :: [LaunchableSmartProgram] -> (SmartProgramCommunicationFunctions -> IO ()) -> IO ()
withRunnableSmartPrograms programs action = withRunnableSmartPrograms' (zip programs ports) action M.empty
    where
        ports = [49152 ..]
        withRunnableSmartPrograms' :: [(LaunchableSmartProgram, Port)] -> (SmartProgramCommunicationFunctions -> IO ()) -> RunningSmartProgramMap -> IO ()
        withRunnableSmartPrograms' ((prog, port) : progs) action accum =
            let programID = lspID prog
            in  withAsync (runSmartProgram prog port) $ \_ -> withRunnableSmartPrograms' progs action (M.insert programID (RunningSmartProgram programID port prog) accum)
        withRunnableSmartPrograms' [] action smartProgramMap =
            let commFunctions = SmartProgramCommunicationFunctions
                    (sendMeasurementStopToRunningSmartPrograms smartProgramMap)
                    (sendImagesToRunningSmartPrograms smartProgramMap)
                    (getRunningSmartProgramDoTimesDecision smartProgramMap)
                    (getRunningSmartProgramStageLoopDecision smartProgramMap)
                    (getRunningSmartProgramRelativeStageLoopDecision smartProgramMap)
                    (getRunningSmartProgramTimeLapseDecision smartProgramMap)
            in  action commFunctions

runSmartProgram :: LaunchableSmartProgram -> Port -> IO ()
runSmartProgram programDetails httpPort =
    withTempFile (lspWorkingDirectory programDetails) "SmartProgramUserCode.py" $ \userCodeFilePath userCodeFileHandle ->
    withTempFile (lspWorkingDirectory programDetails) "SmartProgramWrapper.py" $ \wrapperCodeFilePath wrapperCodeFileHandle ->
    T.hPutStr userCodeFileHandle (lspCode programDetails) >> hFlush userCodeFileHandle >>
    pythonWrapperCode >>= T.hPutStr wrapperCodeFileHandle >> hFlush wrapperCodeFileHandle >>
    let interpreterPath = lspPythonInterpreter programDetails
        commandLine = T.unpack $ formatT "{} {} {}" (interpreterPath, wrapperCodeFilePath, httpPort)
        processParams = (shell commandLine) {std_out = CreatePipe, std_err = CreatePipe}
    in  withCreateProcess processParams $ \_ (Just stdout ) (Just stderr) processHandle ->
        withAsync (forever $ hGetLine stdout >>= \output -> putStrLn ("Output from smart program: " ++ output)) $ \_ ->
        withAsync (forever $ hGetLine stderr >>= \output -> putStrLn ("Error output from smart program: " ++ output)) $ \_ ->
        waitForProcess processHandle >> pure ()

pythonWrapperCode :: IO Text
pythonWrapperCode = T.readFile "SmartProgramWrapperCode.py"

sendMeasurementStopToRunningSmartPrograms :: RunningSmartProgramMap -> IO ()
sendMeasurementStopToRunningSmartPrograms = undefined

sendImagesToRunningSmartPrograms :: RunningSmartProgramMap -> [(AcquisitionMetaData, AcquiredData)] -> [SmartProgramID] -> IO ()
sendImagesToRunningSmartPrograms smartProgramMap images programsToSendTo =
    forConcurrently_ programsToSendTo (\programID ->
        sendImagesToRunningSmartProgram smartProgramMap images programID) 
    where
        sendImagesToRunningSmartProgram :: RunningSmartProgramMap -> [(AcquisitionMetaData, AcquiredData)] -> SmartProgramID -> IO ()
        sendImagesToRunningSmartProgram smartProgramMap images programToSendTo =
            let httpPort = rspPort $ fromJust (M.lookup programToSendTo smartProgramMap)
                allMessages = map (uncurry AcquiredDataMessage) images
                asChannelMessages = map (ChannelMessage 0) allMessages
                asMsgPackLBS = mconcat (map pack asChannelMessages)
                url = http "127.0.0.1" /: "data"
                body = ReqBodyLbs asMsgPackLBS
            in  runReq defaultHttpConfig (
                    liftIO (putStrLn "sending") >>
                    req
                        POST                    -- HTTP method
                        url                     -- URL
                        body                    -- Request body
                        jsonResponse            -- Response type
                        (port httpPort <> responseTimeout 1000000) >>= \response ->
                    pure (responseBody response)) >>= \result ->
                when (not $ isSuccessResponse result) (
                    throwIO $ userError ("sending smart programs image but received " ++ show result))

getRunningSmartProgramDoTimesDecision :: RunningSmartProgramMap -> SmartProgramID -> IO SmartProgramServerResponse
getRunningSmartProgramDoTimesDecision runningPrograms programID = 
    queryRunningProgramForDecision runningPrograms programID "dotimesdecision" >>= \resp ->
    case resp of
        r@ResponseDoTimesDecision{} -> pure r
        ResponseNoDecision          -> pure ResponseNoDecision
        _                           -> throwIO $ userError ("unexpected SmartProgramResponse for do times:" ++ show resp)

getRunningSmartProgramStageLoopDecision :: RunningSmartProgramMap -> SmartProgramID -> IO SmartProgramServerResponse
getRunningSmartProgramStageLoopDecision runningPrograms programID =
    queryRunningProgramForDecision runningPrograms programID "stageloopdecision" >>= \resp ->
    case resp of
        r@ResponseStageLoopDecision{} -> pure r
        ResponseNoDecision            -> pure ResponseNoDecision
        _                             -> throwIO $ userError ("unexpected SmartProgramResponse for stage loop:" ++ show resp)

getRunningSmartProgramRelativeStageLoopDecision :: RunningSmartProgramMap -> SmartProgramID -> IO SmartProgramServerResponse
getRunningSmartProgramRelativeStageLoopDecision runningPrograms programID =
    queryRunningProgramForDecision runningPrograms programID "relativestageloopdecision" >>= \resp ->
    case resp of
        r@ResponseRelativeStageLoopDecision{} -> pure r
        ResponseNoDecision                    -> pure ResponseNoDecision
        _                                     -> throwIO $ userError ("unexpected SmartProgramResponse for relative stage loop:" ++ show resp)

getRunningSmartProgramTimeLapseDecision :: RunningSmartProgramMap -> SmartProgramID -> IO SmartProgramServerResponse
getRunningSmartProgramTimeLapseDecision runningPrograms programID =
    queryRunningProgramForDecision runningPrograms programID "timelapsedecision" >>= \resp ->
    case resp of
        r@ResponseTimeLapseDecision{} -> pure r
        ResponseNoDecision            -> pure ResponseNoDecision
        _                             -> throwIO $ userError ("unexpected SmartProgramResponse for time lapse:" ++ show resp)

queryRunningProgramForDecision :: (FromJSON a) => RunningSmartProgramMap -> SmartProgramID -> Text -> IO a
queryRunningProgramForDecision runningPrograms id path =
    let serverPort = 8100
        url = http "127.0.0.1" /: path
        queryParams = "id" =: (fromSmartProgramID id)
        body = NoReqBody
    in  runReq defaultHttpConfig $
            req
                GET                     -- HTTP method
                url                     -- URL
                body                    -- Request body
                jsonResponse            -- Response type
                (port serverPort <> queryParams <> responseTimeout 1000000) >>= \response -> -- Options (port and query parameter)
            pure (responseBody response)
