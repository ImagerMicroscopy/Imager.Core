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

getSmartProgramDoTimesDecision :: SmartProgramID -> SmartProgramDoTimesDecision
getSmartProgramDoTimesDecision = undefined

getSmartProgramStageLoopDecision :: SmartProgramID -> SmartProgramStageLoopDecision
getSmartProgramStageLoopDecision = undefined

getSmartProgramRelativeStageLoopDecision :: SmartProgramID -> SmartProgramTimeLapseDecision
getSmartProgramRelativeStageLoopDecision = undefined

getSmartProgramTimeLapseDecision :: SmartProgramID -> SmartProgramTimeLapseDecision
getSmartProgramTimeLapseDecision = undefined

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


sendDetectedImageToSmartPrograms_Worker :: SendToSmartProgramsChannel -> IO ()
sendDetectedImageToSmartPrograms_Worker chan =
    putStrLn "Worker starting up" >>
    ((forever (
        readNextImageToSendToSmartPrograms chan >>= \(smartProgramIDs, image) ->    -- may block until an image is available
        putStrLn "Worker has data" >>
        sendImagesToSmartProgramServer [image] smartProgramIDs >>= \response ->
        putStrLn ("received response: " ++ show response) >>
        markImagesSentSuccessfullyToSmartPrograms chan 1)) `catch` (\e -> putStrLn ("Exception caught: " ++ show (e :: SomeException)) >> error (show e)))

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
