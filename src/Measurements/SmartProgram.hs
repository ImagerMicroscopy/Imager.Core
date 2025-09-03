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

startSmartPrograms :: SmartProgramCode -> IO ()
startSmartPrograms code = undefined

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

getSmartProgramDoTimesDecision :: SmartProgramID -> IO (Maybe SmartProgramDoTimesDecision)
getSmartProgramDoTimesDecision id = queryServerForDecision "dotimesdecision" id

getSmartProgramStageLoopDecision :: SmartProgramID -> IO (Maybe SmartProgramStageLoopDecision)
getSmartProgramStageLoopDecision = undefined

getSmartProgramRelativeStageLoopDecision :: SmartProgramID -> IO (Maybe SmartProgramRelativeStageLoopDecision)
getSmartProgramRelativeStageLoopDecision = undefined

getSmartProgramTimeLapseDecision :: SmartProgramID -> IO (Maybe SmartProgramTimeLapseDecision)
getSmartProgramTimeLapseDecision = undefined

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

parseSmartProgramIDsFromProgramCode :: SmartProgramCode -> [SmartProgramID]
parseSmartProgramIDsFromProgramCode code =
    case (decode (LB.fromStrict . T.encodeUtf8 $ fromSmartProgramCode code) :: Maybe Array) of
        Just (objs :: V.Vector Value) -> map findID (V.toList objs)
        _                    -> error "Could not parse smart program ids"
    where
        findID :: Value -> SmartProgramID
        findID (Object o) = case (flip parseMaybe o $ \x -> x .: "DagID") of
                                Just id -> SmartProgramID id
                                Nothing -> error "Could not parse smart program id"
        findID _          = error "did not find object encoding the smart program id"

data SmartSendResponse = SmartSendResponseStatusOK
                       | SmartSendResponseStatusError
                        deriving (Show)

instance FromJSON SmartSendResponse where
    parseJSON (Object v) =
        v .: "type" >>= \(t :: Text) ->
        case (T.toLower t) of
            "status" -> v .: "status" >>= \(status :: Text) -> case status of "ok"    -> pure SmartSendResponseStatusOK
                                                                              "error" -> pure SmartSendResponseStatusError

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
                putStrLn "Worker has data" >>
                sendImagesToSmartProgramServer [image] smartProgramIDs >>= \response ->
                putStrLn ("received response: " ++ show response) >>
                readWaitableChannel chan -- remove the item from the queue
            )

sendImagesToSmartProgramServer :: [(AcquisitionMetaData, AcquiredData)] -> [SmartProgramID] -> IO SmartSendResponse
sendImagesToSmartProgramServer images ids =
    let allMessages = map (uncurry AcquiredDataMessage)  images
        asChannelMessages = map (ChannelMessage 0) allMessages
        asMsgPackLBS = mconcat (map pack asChannelMessages)
        serverPort = 8100
        url = http "127.0.0.1" /: "data"
        queryParams = mconcat [ "id" =: (fromSmartProgramID id) | id <- ids ]
        body = ReqBodyLbs asMsgPackLBS
    in  runReq defaultHttpConfig $ 
            liftIO (putStrLn "sending") >>
            req
                POST                    -- HTTP method
                url                     -- URL
                body                    -- Request body
                jsonResponse            -- Response type
                (port serverPort <> queryParams <> responseTimeout 1000000) >>= \response -> -- Options (port and query parameter)
            pure (responseBody response)
