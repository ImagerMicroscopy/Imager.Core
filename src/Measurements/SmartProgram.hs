{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Measurements.SmartProgram (
    getAllSmartProgramIDsUsedInMeasurement
  , getUpdatedAcquisitionDecision
  , withSmartProgramServer
  , sendDetectedImageToSmartPrograms_Worker
  , parseSmartProgramIDsFromCode
) where

import Control.Concurrent.STM
import Control.Exception
import qualified Control.Exception.Safe as SE
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import Data.Maybe
import Data.MessagePack
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as S
import qualified Data.Vector as V
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Lazy.Char8 as LBS



import Measurements.MeasurementProgramTypes
import Measurements.MeasurementProgramEncoding
import Utils.WaitableChannel

getAllSmartProgramIDsUsedInMeasurement :: MeasurementElement -> [SmartProgramID]
getAllSmartProgramIDsUsedInMeasurement me = S.toList (searchWorker S.empty me)
    where
        searchWorker :: S.Set SmartProgramID -> MeasurementElement -> S.Set SmartProgramID
        searchWorker s (MEDetection _ _ ids) = S.fromList ids
        searchWorker s (MEIrradiation{}) = s
        searchWorker s (MEWait{}) = s
        searchWorker s (MEFastAcquisitionLoop _ _ _ sid ids) = S.fromList ids <> if (isJust sid) then S.singleton (fromJust sid) else S.empty
        searchWorker s (MEExecuteRobotProgram{}) = s
        searchWorker s (MEDoTimes _ _ sid es) = mconcat (map (searchWorker s) es) <> if (isJust sid) then S.singleton (fromJust sid) else S.empty
        searchWorker s (METimeLapse _ _ _ sid es) = mconcat (map (searchWorker s) es) <> if (isJust sid) then S.singleton (fromJust sid) else S.empty
        searchWorker s (MEStageLoop _ _ _ sid es) = mconcat (map (searchWorker s) es) <> if (isJust sid) then S.singleton (fromJust sid) else S.empty
        searchWorker s (MERelativeStageLoop _ _ _ sid es) = mconcat (map (searchWorker s) es) <> if (isJust sid) then S.singleton (fromJust sid) else S.empty

withSmartProgramServer :: SmartProgramCode -> (SmartProgramCommunicationFunctions -> IO ()) -> IO ()
withSmartProgramServer programs action = 
    sendProgramsToSmartProgramServer programs >>
    (action smartProgramServerCommunicationFunctions) `finally` (sendMeasurementFinishedToSmartProgramServer `SE.catchAny` (\_ -> pure ()))

smartProgramServerCommunicationFunctions :: SmartProgramCommunicationFunctions
smartProgramServerCommunicationFunctions =
    SmartProgramCommunicationFunctions
        sendImagesToSmartProgramServer
        getSmartProgramDoTimesDecision
        getSmartProgramStageLoopDecision
        getSmartProgramRelativeStageLoopDecision
        getSmartProgramTimeLapseDecision


getUpdatedAcquisitionDecision :: SmartProgramID -> ElementID -> DefinedDetections -> IO (Maybe DefinedDetections)
getUpdatedAcquisitionDecision programID elementID ddets = do
    let serverPort = 5100
        url = http "127.0.0.1" /: "updateacquisitionparams"
        queryParams = "dagid" =: fromSmartProgramID programID  <> 
                      "elementid" =: fromElementID elementID
        body = ReqBodyJson ddets

    runReq defaultHttpConfig $ do
        liftIO $ LBS.putStrLn (encode ddets)
        response <- req
            POST
            url
            body
            lbsResponse  
            (port serverPort <> queryParams <> responseTimeout 1000000)
        let result = responseBody response
        pure (decode result :: Maybe DefinedDetections)

getSmartProgramDoTimesDecision :: SmartProgramID -> IO SmartProgramServerResponse
getSmartProgramDoTimesDecision programID = 
    queryServerForDecision "dotimesdecision" programID >>= \resp ->
    case resp of
        r@ResponseDoTimesDecision{} -> pure r
        r@ResponseNoDecision{}      -> pure r
        _                           -> throwIO $ userError ("unexpected SmartProgramResponse for do times:" ++ show resp)

getSmartProgramStageLoopDecision :: SmartProgramID -> IO SmartProgramServerResponse
getSmartProgramStageLoopDecision programID =
    queryServerForDecision "stageloopdecision" programID >>= \resp ->
    case resp of
        r@ResponseStageLoopDecision{} -> pure r
        r@ResponseNoDecision{}        -> pure r
        _                             -> throwIO $ userError ("unexpected SmartProgramResponse for stage loop:" ++ show resp)

getSmartProgramRelativeStageLoopDecision :: SmartProgramID -> IO SmartProgramServerResponse
getSmartProgramRelativeStageLoopDecision programID =
    queryServerForDecision "relativestageloopdecision" programID >>= \resp ->
    case resp of
        r@ResponseRelativeStageLoopDecision{} -> pure r
        r@ResponseNoDecision{}                -> pure r
        _                                     -> throwIO $ userError ("unexpected SmartProgramResponse for relative stage loop:" ++ show resp)

getSmartProgramTimeLapseDecision :: SmartProgramID -> IO SmartProgramServerResponse
getSmartProgramTimeLapseDecision programID =
    queryServerForDecision "timelapsedecision" programID >>= \resp ->
    case resp of
        r@ResponseTimeLapseDecision{} -> pure r
        r@ResponseNoDecision{}        -> pure r
        _                             -> throwIO $ userError ("unexpected SmartProgramResponse for time lapse:" ++ show resp)

queryServerForDecision :: (FromJSON a) => Text -> SmartProgramID -> IO a
queryServerForDecision path id =
    let serverPort = 5100
        url = http "127.0.0.1" /: "decisions" /: "get_decision" /: path
        queryParams = "dagid" =: (fromSmartProgramID id)
        body = NoReqBody
    in  runReq defaultHttpConfig $
            req
                GET                     -- HTTP method
                url                     -- URL
                body                    -- Request body
                jsonResponse            -- Response type
                (port serverPort <> queryParams <> responseTimeout 1000000) >>= \response -> -- Options (port and query parameter)
            pure (responseBody response)

sendProgramsToSmartProgramServer :: SmartProgramCode -> IO ()
sendProgramsToSmartProgramServer code =
    let serverPort = 5100
        url = http "127.0.0.1" /: "nodesubmission" /: "set_dags"
        body = ReqBodyJson code
    in  runReq defaultHttpConfig ( 
            liftIO (LBS.putStrLn (encode code)) >>
            req
                POST                    -- HTTP method
                url                     -- URL
                body                    -- Request body
                jsonResponse            -- Response type
                (port serverPort <> responseTimeout 1000000) >>= \response -> -- Options (port and query parameter)
            pure (responseBody response)) >>= \result ->
        when (not $ isSuccessResponse result) (
            throwIO $ userError ("sending smart programs but received " ++ show result))


sendImagesToSmartProgramServer :: [(AcquisitionMetaData, AcquiredData)] -> [SmartProgramID] -> IO ()
sendImagesToSmartProgramServer images ids =
    let allMessages = map (uncurry AcquiredDataMessage)  images
        asChannelMessages = map (ChannelMessage 0) allMessages
        asMsgPackLBS = mconcat (map pack asChannelMessages)
        serverPort = 5100
        url = http "127.0.0.1" /: "submit_data"
        queryParams = mconcat [ "dagid" =: (fromSmartProgramID id) | id <- ids ]
        body = ReqBodyLbs asMsgPackLBS
    in  runReq defaultHttpConfig (
            liftIO (putStrLn "sending images to smart program server") >>
            req
                POST                    -- HTTP method
                url                     -- URL
                body                    -- Request body
                jsonResponse            -- Response type
                (port serverPort <> queryParams <> responseTimeout 10000000) >>= \response -> -- Options (port and query parameter)
            pure (responseBody response)) >>= \result ->
        when (not $ isSuccessResponse result) (
            throwIO $ userError ("sending smart programs image but received " ++ show result))

sendMeasurementFinishedToSmartProgramServer :: IO ()
sendMeasurementFinishedToSmartProgramServer =
    -- send the signal but do not throw any exceptions even if this fail - when we 
    -- send this we are already cleaning up anyway.
    sendMessage `catch` (\e -> putStrLn ("Exception caught in stop send: " ++ show (e :: SomeException)))
    where
        sendMessage =
            let serverPort = 5100
                url = http "127.0.0.1" /: "measurementfinished"
                body = NoReqBody
            in  runReq defaultHttpConfig (
                    req
                        POST                    -- HTTP method
                        url                     -- URL
                        body                    -- Request body
                        jsonResponse            -- Response type
                        (port serverPort <> responseTimeout 1000000) >>= \response -> -- Options (port and query parameter)
                    pure (responseBody response))

parseSmartProgramIDsFromCode :: SmartProgramCode -> [SmartProgramID]
parseSmartProgramIDsFromCode (DAGOrchestratorCode code) =
    case code of
        (Array objs) -> map findID (V.toList objs)
        _            -> error "Could not parse smart program ids"
    where
        findID :: Value -> SmartProgramID
        findID (Object o) = case (flip parseMaybe o $ \x -> x .: "DagID") of
                                Just id -> SmartProgramID id
                                Nothing -> error "Could not parse smart program id"
        findID _          = error "did not find object encoding the smart program id"
parseSmartProgramIDsFromCode (ProgramRunnerCode cs) = map lspID cs


sendDetectedImageToSmartPrograms_Worker :: ([(AcquisitionMetaData, AcquiredData)] -> [SmartProgramID] -> IO ()) -> SendToSmartProgramsChannelReader -> IO ()
sendDetectedImageToSmartPrograms_Worker sendFunc chan =
    putStrLn "Worker starting up" >>
    loopRead `catch` (\e -> putStrLn ("Exception caught in image send: " ++ show (e :: SomeException)) >> throwIO e)
    where
        loopRead =
            forever (
                peekWaitableChannel chan >>= \(smartProgramIDs, image) ->
                sendFunc [image] smartProgramIDs >>
                readWaitableChannel chan -- remove the item from the queue
            )

