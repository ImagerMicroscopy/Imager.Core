{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Measurements.SmartProgram where

import Control.Concurrent.STM
import Control.Exception
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

import Measurements.MeasurementProgramTypes
import Utils.WaitableChannel

getAllSmartProgramIDsUsedInMeasurement :: MeasurementElement -> [SmartProgramID]
getAllSmartProgramIDsUsedInMeasurement me = S.toList (searchWorker S.empty me)
    where
        searchWorker :: S.Set SmartProgramID -> MeasurementElement -> S.Set SmartProgramID
        searchWorker s (MEDetection _ ids) = S.fromList ids
        searchWorker s (MEIrradiation _ _) = s
        searchWorker s (MEWait _) = s
        searchWorker s (MEFastAcquisitionLoop _ _ sid ids) = S.fromList ids <> if (isJust sid) then S.singleton (fromJust sid) else S.empty
        searchWorker s (MEExecuteRobotProgram _ _ _) = s
        searchWorker s (MEDoTimes _ sid es) = mconcat (map (searchWorker s) es) <> if (isJust sid) then S.singleton (fromJust sid) else S.empty
        searchWorker s (METimeLapse _ _ sid es) = mconcat (map (searchWorker s) es) <> if (isJust sid) then S.singleton (fromJust sid) else S.empty
        searchWorker s (MEStageLoop _ _ sid es) = mconcat (map (searchWorker s) es) <> if (isJust sid) then S.singleton (fromJust sid) else S.empty
        searchWorker s (MERelativeStageLoop _ _ sid es) = mconcat (map (searchWorker s) es) <> if (isJust sid) then S.singleton (fromJust sid) else S.empty

data SmartProgramServerResponse = ResponseSuccess
                                | ResponseError !Text
                                | ResponseNoDecision
                                | ResponseDoTimesDecision !NumIterationsTotal
                                | ResponseStageLoopDecision ![PositionNameAndCoords]
                                | ResponseRelativeStageLoopDecision !RelativeStageLoopParams
                                | ResponseTimeLapseDecision !NumIterationsTotal !WaitDuration
                                deriving (Show)

isNoDecisionResponse :: SmartProgramServerResponse -> Bool
isNoDecisionResponse ResponseNoDecision = True
isNoDecisionResponse _                  = False

isSuccessResponse :: SmartProgramServerResponse -> Bool
isSuccessResponse ResponseSuccess = True
isSuccessResponse _               = False

instance FromJSON SmartProgramServerResponse where
    parseJSON (Object v) = 
        v .: "type" >>= \(tt :: Text) ->
        case tt of
            "status"                    -> v .: "status" >>= \(st :: Text) -> 
                                               case st of
                                                   "success" -> pure ResponseSuccess
                                                   "error"   -> ResponseError <$> v .: "what"
            "nodecision"                -> pure ResponseNoDecision
            "dotimesdecision"           -> ResponseDoTimesDecision <$> v .: "nototal"
            "stageloopdecision"         -> ResponseStageLoopDecision <$> v .: "positions"
            "relativestageloopdecision" -> ResponseRelativeStageLoopDecision <$> v .: "params"
            "timelapsedecision"         -> ResponseTimeLapseDecision <$> v .: "ntotal" <*> v .: "timedelta"
            _                           -> fail "unknown SmartProgramServerResponse"

getSmartProgramDoTimesDecision :: SmartProgramID -> IO SmartProgramServerResponse
getSmartProgramDoTimesDecision programID = 
    queryServerForDecision "dotimesdecision" programID >>= \resp ->
    case resp of
        r@ResponseDoTimesDecision{} -> pure r
        ResponseNoDecision          -> pure ResponseNoDecision
        _                           -> throwIO $ userError ("unexpected SmartProgramResponse for do times:" ++ show resp)

getSmartProgramStageLoopDecision :: SmartProgramID -> IO SmartProgramServerResponse
getSmartProgramStageLoopDecision programID =
    queryServerForDecision "stageloopdecision" programID >>= \resp ->
    case resp of
        r@ResponseStageLoopDecision{} -> pure r
        ResponseNoDecision            -> pure ResponseNoDecision
        _                             -> throwIO $ userError ("unexpected SmartProgramResponse for stage loop:" ++ show resp)

getSmartProgramRelativeStageLoopDecision :: SmartProgramID -> IO SmartProgramServerResponse
getSmartProgramRelativeStageLoopDecision programID =
    queryServerForDecision "relativestageloopdecision" programID >>= \resp ->
    case resp of
        r@ResponseRelativeStageLoopDecision{} -> pure r
        ResponseNoDecision                    -> pure ResponseNoDecision
        _                                     -> throwIO $ userError ("unexpected SmartProgramResponse for relative stage loop:" ++ show resp)

getSmartProgramTimeLapseDecision :: SmartProgramID -> IO SmartProgramServerResponse
getSmartProgramTimeLapseDecision programID =
    queryServerForDecision "timelapsedecision" programID >>= \resp ->
    case resp of
        r@ResponseTimeLapseDecision{} -> pure r
        ResponseNoDecision            -> pure ResponseNoDecision
        _                             -> throwIO $ userError ("unexpected SmartProgramResponse for time lapse:" ++ show resp)

queryServerForDecision :: (FromJSON a) => Text -> SmartProgramID -> IO a
queryServerForDecision path id =
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

sendProgramsToSmartProgramServer :: SmartProgramCode -> IO ()
sendProgramsToSmartProgramServer code =
    let serverPort = 8100
        url = http "127.0.0.1" /: "startprograms"
        body = ReqBodyJson code
    in  runReq defaultHttpConfig ( 
            liftIO (putStrLn "sending") >>
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
        serverPort = 8100
        url = http "127.0.0.1" /: "data"
        queryParams = mconcat [ "id" =: (fromSmartProgramID id) | id <- ids ]
        body = ReqBodyLbs asMsgPackLBS
    in  runReq defaultHttpConfig (
            liftIO (putStrLn "sending") >>
            req
                POST                    -- HTTP method
                url                     -- URL
                body                    -- Request body
                jsonResponse            -- Response type
                (port serverPort <> queryParams <> responseTimeout 1000000) >>= \response -> -- Options (port and query parameter)
            pure (responseBody response)) >>= \result ->
        when (not $ isSuccessResponse result) (
            throwIO $ userError ("sending smart programs image but received " ++ show result))

emptySmartProgramCode :: SmartProgramCode
emptySmartProgramCode = SmartProgramCode (Array V.empty)

parseSmartProgramIDsFromProgramCode :: SmartProgramCode -> [SmartProgramID]
parseSmartProgramIDsFromProgramCode code =
    case (fromSmartProgramCode code) of
        (Array objs) -> map findID (V.toList objs)
        _                    -> error "Could not parse smart program ids"
    where
        findID :: Value -> SmartProgramID
        findID (Object o) = case (flip parseMaybe o $ \x -> x .: "DagID") of
                                Just id -> SmartProgramID id
                                Nothing -> error "Could not parse smart program id"
        findID _          = error "did not find object encoding the smart program id"

type SendToSmartProgramsChannelWriter = WaitableChannelWriter ([SmartProgramID], (AcquisitionMetaData, AcquiredData))
type SendToSmartProgramsChannelReader = WaitableChannelReader ([SmartProgramID], (AcquisitionMetaData, AcquiredData))

sendDetectedImageToSmartPrograms_Worker :: SendToSmartProgramsChannelReader -> IO ()
sendDetectedImageToSmartPrograms_Worker chan =
    putStrLn "Worker starting up" >>
    loopRead `catch` (\e -> putStrLn ("Exception caught: " ++ show (e :: SomeException)) >> throwIO e)
    where
        loopRead =
            forever (
                peekWaitableChannel chan >>= \(smartProgramIDs, image) ->
                putStrLn "Smart server send worker has data" >>
                sendImagesToSmartProgramServer [image] smartProgramIDs >>
                readWaitableChannel chan -- remove the item from the queue
            )

