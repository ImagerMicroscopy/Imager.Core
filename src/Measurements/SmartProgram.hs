module Measurements.SmartProgram where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V

import Measurements.MeasurementProgramTypes

getSmartProgramDoTimesDecision :: SmartProgramID -> SmartProgramDoTimesDecision
getSmartProgramDoTimesDecision = undefined

getSmartProgramStageLoopDecision :: SmartProgramID -> SmartProgramStageLoopDecision
getSmartProgramStageLoopDecision = undefined

getSmartProgramRelativeStageLoopDecision :: SmartProgramID -> SmartProgramTimeLapseDecision
getSmartProgramRelativeStageLoopDecision = undefined

getSmartProgramTimeLapseDecision :: SmartProgramID -> SmartProgramTimeLapseDecision
getSmartProgramTimeLapseDecision = undefined
    
sendDetectedImagesToSmartProgram :: AsyncMeasurementMessage -> SmartProgramID -> IO ()
sendDetectedImagesToSmartProgram = undefined


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


