module Measurements.SmartProgramRunner (
    withRunnableSmartPrograms
  , emptySmartProgramCode
  , formatExampleCodeForIgorProcedure
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import qualified Control.Exception.Safe as SE
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.MessagePack
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Req
import System.Directory
import System.Environment
import System.FilePath
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import System.Timeout

import Measurements.MeasurementProgramTypes
import Measurements.MeasurementProgramEncoding
import Utils.MiscUtils

data RunningSmartProgram = RunningSmartProgram {
                               rspID :: !SmartProgramID
                             , rspPort :: !Port
                             , rspProgram :: !LaunchableSmartProgram
                        } deriving (Show)

type RunningSmartProgramMap = M.Map SmartProgramID RunningSmartProgram

emptySmartProgramCode :: SmartProgramCode
emptySmartProgramCode = ProgramRunnerCode []

withRunnableSmartPrograms :: [LaunchableSmartProgram] -> (SmartProgramCommunicationFunctions -> IO ()) -> IO ()
withRunnableSmartPrograms programs action =
    sequence (take (length programs) (repeat newEmptyMVar)) >>= \programsRunningFlags ->
    let programsPortsAndFlags = zip3 programs [49152..] programsRunningFlags
        runningPrograms = map (\(prog, port, _) -> (lspID prog, RunningSmartProgram (lspID prog) port prog)) programsPortsAndFlags
        smartProgramMap = M.fromList runningPrograms
        commFunctions = SmartProgramCommunicationFunctions
            (sendImagesToRunningSmartPrograms smartProgramMap)
            (getRunningSmartProgramDoTimesDecision smartProgramMap)
            (getRunningSmartProgramStageLoopDecision smartProgramMap)
            (getRunningSmartProgramRelativeStageLoopDecision smartProgramMap)
            (getRunningSmartProgramTimeLapseDecision smartProgramMap)
    in  withAsync ((mapConcurrently_ runSmartProgram programsPortsAndFlags) `catch` (\e -> putStrLn ("Exception caught in withRunnableSmartPrograms: " ++ show (e :: SomeException)) >> throwIO e)) $ \_ ->
        timeout 10000000 (mapM_ takeMVar programsRunningFlags) >>= \result ->
        case result of
            Nothing -> throwIO $ userError "timeout starting smart programs"
            Just _  -> action commFunctions

runSmartProgram :: (LaunchableSmartProgram, Port, MVar Bool) -> IO ()
runSmartProgram (programDetails, httpPort, isRunningFlag) =
    doIt `catch` (\e -> putStrLn ("Exception caught in runSmartProgram: " ++ show (e :: SomeException)) >> throwIO e)
    where
        withTempFileF = if (null (lspWorkingDirectory programDetails)) then withSystemTempFile else (withTempFile (lspWorkingDirectory programDetails))
        doIt =
            withTempFileF "SmartProgramUserCode.py" (\userCodeFilePath userCodeFileHandle ->
            withTempFileF "SmartProgramWrapper.py" (\wrapperCodeFilePath wrapperCodeFileHandle ->
            T.hPutStr userCodeFileHandle (lspCode programDetails) >> hFlush userCodeFileHandle >>
            readPythonWrapperCode >>= T.hPutStr wrapperCodeFileHandle >> hFlush wrapperCodeFileHandle >>
            putStrLn ("Starting program on port " ++ show httpPort) >>
            let interpreterPath = lspPythonInterpreter programDetails
                processParams = (proc interpreterPath ["-u", wrapperCodeFilePath, show httpPort, userCodeFilePath]) {std_out = CreatePipe, std_err = CreatePipe}
                -- the "-u" flag makes stdout and stderr unbuffered, so that we can read output as it is produced
                startProcess :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
                startProcess = createProcess processParams
                processHandler :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
                processHandler (_, (Just stdout), (Just stderr), processHandle) =
                    waitForStart stderr >> putMVar isRunningFlag True >>
                    withAsync (readAndPrint stdout) ( \h ->
                    withAsync (readAndPrint stderr) ( \_ ->
                    wait h >> pure ()))
                stopProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
                stopProcess args =
                    (sendShutdownToRunningSmartProgram httpPort) `SE.catchAny` (\_ -> pure ()) >>
                    cleanupProcess args >> pure ()
                waitForStart :: Handle -> IO ()
                waitForStart h = hGetLineInterruptible h >>= \contents ->
                                 if ("Running on http://" `isInfixOf` contents)
                                 then pure () else putStrLn contents >> waitForStart h
                hGetLineInterruptible :: Handle -> IO String    -- exists because hGetLine cannot be aborted by an asynchronous exception
                hGetLineInterruptible h = hGetLineInterruptible' h []
                    where
                        hGetLineInterruptible' h accum = 
                            hWaitForInput h 100 >>= \result ->
                            case result of
                                False -> hGetLineInterruptible' h accum
                                True  -> hGetChar h >>= \char ->
                                         if (char == '\n')
                                         then pure (reverse accum)
                                         else hGetLineInterruptible' h (char : accum)
                readAndPrint :: Handle -> IO ()
                readAndPrint h = forever $ hGetLineInterruptible h >>= \output -> when (not (null output)) (putStrLn ("Output from smart program: " ++ output))
                in  bracket startProcess stopProcess processHandler))
    
readPythonWrapperCode :: IO Text
readPythonWrapperCode = takeDirectory <$> getExecutablePath >>= \imagerDir ->
    let filepath = imagerDir </> "SmartProgramSupport" </> "SmartProgramRunner.py"
    in  putStrLn "will read code " >> T.readFile filepath >>= \code -> putStrLn "have code" >> pure code

sendShutdownToRunningSmartProgram :: Port -> IO ()
sendShutdownToRunningSmartProgram httpPort =
    let url = http "127.0.0.1" /: "shutdown"
        body = NoReqBody
    in  runReq defaultHttpConfig (
            liftIO (putStrLn "sending shutdown to running program") >>
            req
                POST                    -- HTTP method
                url                     -- URL
                body                    -- Request body
                jsonResponse            -- Response type
                (port httpPort <> responseTimeout 1000000) >>= \response ->
            pure (responseBody response)) >>= \result ->
        when (not $ isSuccessResponse result) (
            throwIO $ userError ("sending smart programs shutdown but received " ++ show result))

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
            in  putStrLn ("sending " ++ show (length images) ++ " images to smart program") >>
                runReq defaultHttpConfig (
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
        r@ResponseNoDecision{}      -> pure r
        _                           -> throwIO $ userError ("unexpected SmartProgramResponse for do times:" ++ show resp)

getRunningSmartProgramStageLoopDecision :: RunningSmartProgramMap -> SmartProgramID -> IO SmartProgramServerResponse
getRunningSmartProgramStageLoopDecision runningPrograms programID =
    queryRunningProgramForDecision runningPrograms programID "stageloopdecision" >>= \resp ->
    case resp of
        r@ResponseStageLoopDecision{} -> pure r
        r@ResponseNoDecision{}        -> pure r
        _                             -> throwIO $ userError ("unexpected SmartProgramResponse for stage loop:" ++ show resp)

getRunningSmartProgramRelativeStageLoopDecision :: RunningSmartProgramMap -> SmartProgramID -> IO SmartProgramServerResponse
getRunningSmartProgramRelativeStageLoopDecision runningPrograms programID =
    queryRunningProgramForDecision runningPrograms programID "relativestageloopdecision" >>= \resp ->
    case resp of
        r@ResponseRelativeStageLoopDecision{} -> pure r
        r@ResponseNoDecision{}                -> pure r
        _                                     -> throwIO $ userError ("unexpected SmartProgramResponse for relative stage loop:" ++ show resp)

getRunningSmartProgramTimeLapseDecision :: RunningSmartProgramMap -> SmartProgramID -> IO SmartProgramServerResponse
getRunningSmartProgramTimeLapseDecision runningPrograms programID =
    queryRunningProgramForDecision runningPrograms programID "timelapsedecision" >>= \resp ->
    case resp of
        r@ResponseTimeLapseDecision{} -> pure r
        r@ResponseNoDecision{}        -> pure r
        _                             -> throwIO $ userError ("unexpected SmartProgramResponse for time lapse:" ++ show resp)

queryRunningProgramForDecision :: (FromJSON a) => RunningSmartProgramMap -> SmartProgramID -> Text -> IO a
queryRunningProgramForDecision smartProgramMap programToSendTo path =
    let httpPort = rspPort $ fromJust (M.lookup programToSendTo smartProgramMap)
        url = http "127.0.0.1" /: path
        body = NoReqBody
    in  runReq defaultHttpConfig $
            req
                GET                     -- HTTP method
                url                     -- URL
                body                    -- Request body
                jsonResponse            -- Response type
                (port httpPort <> responseTimeout 1000000) >>= \response -> -- Options (port and query parameter)
            pure (responseBody response)

-- helper function that can be used from GHCI
formatExampleCodeForIgorProcedure :: Text -> Text
formatExampleCodeForIgorProcedure =
    T.unlines . map (\l -> "\tdefaultCode += \"" <> l <> "\\n\"") . map (T.replace "\"" "\\\"") . T.lines
