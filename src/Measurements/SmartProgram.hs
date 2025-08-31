{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Measurements.SmartProgram where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import Data.MessagePack
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as S
import qualified Data.Vector as V
import GHC.Generics
import Network.HTTP.Req

import Measurements.MeasurementProgramTypes

getAllSmartProgramIDsUsedInMeasurement :: MeasurementElement -> [SmartProgramID]
getAllSmartProgramIDsUsedInMeasurement me = S.toList (searchWorker S.empty me)
    where
        searchWorker :: S.Set SmartProgramID -> MeasurementElement -> S.Set SmartProgramID
        searchWorker s (MEDetection _ ids) = foldl' (flip S.insert) s ids
        searchWorker s (MEIrradiation _ _) = s
        searchWorker s (MEWait _) = s
        searchWorker s (MEFastAcquisitionLoop _ _ ) = s
        searchWorker s (MEExecuteRobotProgram _ _ _) = s
        searchWorker s (MEDoTimes _ es) = mconcat (map (searchWorker s) es)
        searchWorker s (METimeLapse _ _ es) = mconcat (map (searchWorker s) es)
        searchWorker s (MEStageLoop _ _ es) = mconcat (map (searchWorker s) es)
        searchWorker s (MERelativeStageLoop _ _ es) = mconcat (map (searchWorker s) es)

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

data SendResponse = ResponseOK
                deriving (Generic, FromJSON)

sendDetectedImagesToSmartProgram_Worker :: SendToSmartProgramsChannel -> IO ()
sendDetectedImagesToSmartProgram_Worker chan =
    readNextImagesToSendToSmartPrograms chan >>= \(smartProgramIds, images) ->
    sendDetectedImagesToSmartProgramServer images smartProgramIds >>
    markImageSetSuccessfullySentToSmartPrograms chan

sendDetectedImagesToSmartProgramServer :: [(AcquisitionMetaData, AcquiredData)] -> [SmartProgramID] -> IO SendResponse
sendDetectedImagesToSmartProgramServer dat ids =
    let acqMessages = map (\(md, d) -> AcquiredDataMessage md d) dat
        channelMessages = map (ChannelMessage 0) acqMessages
        asMsgPack = mconcat (map pack channelMessages)
        serverPort = 8100
        url = http "localhost" /: "data"
        queryParams = mconcat [ "id" =: (fromSmartProgramID id) | id <- ids ]
        body = ReqBodyLbs asMsgPack
    in  runReq defaultHttpConfig $ 
            req
                POST                    -- HTTP method
                url                     -- URL
                body                    -- Request body
                jsonResponse            -- Response type
                (port serverPort <> queryParams) >>= -- Options (port and query parameter)
            liftIO . pure . responseBody
