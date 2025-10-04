{-# LANGUAGE OverloadedStrings #-}

module Measurements.MeasurementProgramEncoding
where

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Data.MessagePack
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Generics
import System.Clock
import Equipment.Equipment
import Equipment.EquipmentTypes
import Camera.SCCameraTypes
import Utils.MiscUtils
import Measurements.MeasurementProgramTypes

instance FromJSON MeasurementElement where
    parseJSON (Object v) =
        v .: "elementtype" >>= \(typ :: Text) ->
        case typ of
          "detection"   -> MEDetection <$> v .: "elementid" <*> v .: "detectionnames" <*> v .: "smartprogramids"
          "irradiation" -> MEIrradiation <$> v .: "elementid" <*> v .: "duration" <*> v .: "irradiation"
          "wait"        -> MEWait <$> v .: "elementid" <*> v .: "duration"
          "executerobotprogram" -> MEExecuteRobotProgram <$> v .: "elementid" <*> v .: "programparameters"
          "dotimes"     -> MEDoTimes <$> v .: "elementid" <*> v .: "ntotal" <*> v .: "smartprogramid" <*> v .: "elements"
          "timelapse"   -> METimeLapse <$> v .: "elementid" <*> v .: "ntotal" <*> v .: "timedelta" <*> v .: "smartprogramid" <*> v .: "elements"
          -- no FromJSON instance for MEFastAcquisitionLoop because it is automatically applied
          "stageloop"   -> MEStageLoop <$> v .: "elementid" <*> v .: "stagename" <*> v .: "positions" <*> v .: "smartprogramid" <*> v .: "elements"
          "relativestageloop" -> MERelativeStageLoop <$> v .: "elementid" <*> v .: "stagename" <*> v .: "params" <*> v .: "smartprogramid" <*> v .: "elements"
          _             -> fail "can't decode measurement element type"
    parseJSON _ = fail "can't decode measurement element"
instance ToJSON MeasurementElement where
  toEncoding (MEDetection eid dets programIDs) = pairs ("elementtype" .= ("detection" :: Text) <> "elementid" .= eid <> "detectionnames" .= dets <> "smartprogramids" .= programIDs)
  toEncoding (MEIrradiation eid dur ip) = pairs ("elementtype" .= ("irradiation" :: Text) <> "elementid" .= eid <> "duration" .= dur <> "irradiation" .= ip)
  toEncoding (MEWait eid d) = pairs ("elementtype" .= ("wait" :: Text) <> "elementid" .= eid <> "duration" .= d)
  toEncoding (MEExecuteRobotProgram eid params) = pairs ("elementtype" .= ("executerobotprogram" :: Text) <> "elementid" .= eid <> "programparameters" .= params)
  toEncoding (MEDoTimes eid n ids es) = pairs ("elementtype" .= ("dotimes" :: Text) <> "elementid" .= eid <> "ntotal" .= n <> "smartprogramid" .= ids <> "elements" .= es)
  toEncoding (METimeLapse eid n td ids  es) = pairs ("elementtype" .= ("timelapse" :: Text) <> "elementid" .= eid <> "ntotal" .= n <> "timedelta" .= td <> "smartprogramid" .= ids <>  "elements" .= es)
  toEncoding (MEFastAcquisitionLoop eid n det inputProgramID programIDs) = pairs ("elementtype" .= ("fastacquisitionloop" :: Text) <> "elementid" .= eid <> "ntotal" .= n <> "detection" .= det <> "smartprogramid" .= inputProgramID <> "smartprogramids" .= programIDs)
  toEncoding (MEStageLoop eid n pos ids  es) = pairs ("elementtype" .= ("stageloop" :: Text) <> "elementid" .= eid <> "stagename" .= n <> "positions" .= pos <> "smartprogramid" .= ids <> "elements" .= es)
  toEncoding (MERelativeStageLoop eid n ps ids  es) = pairs ("elementtype" .= ("relativestageloop" :: Text) <> "elementid" .= eid <> "stagename" .= n <> "params" .= ps <> "smartprogramid" .= ids <> "elements" .= es)
  toJSON _ = error "no toJSON"

instance ToJSON ElementID where
    toJSON (ElementID i) = toJSON i
instance FromJSON ElementID where
    parseJSON = fmap ElementID . parseJSON
instance MessagePack ElementID where
    toObject (ElementID i) = toObject i
    fromObject o = ElementID <$> (fromObject o)    

instance FromJSON DetectionParams where
    parseJSON (Object v) =
        DetectionParams <$> v .: "detectors"
                        <*> v .: "irradiation"
                        <*> v .: "movablecomponents"
    parseJSON _ = fail "can't decode detection params"
instance ToJSON DetectionParams where
    toJSON (DetectionParams detectors irr movableComponents) =
        object ["detectors" .= detectors, "irradiation" .= irr, "movablecomponents" .= movableComponents]

instance FromJSON DetectorParams where
    parseJSON (Object v) =
        DetectorParams <$> v .: "detectorname"
                       <*> v .: "detectorproperties"
    parseJSON _ = fail "can't decode detection params"
instance ToJSON DetectorParams where
    toJSON (DetectorParams name options) =
        object ["detectorname" .= name, "detectorproperties" .= options]

instance FromJSON IrradiationParams where
    parseJSON (Object v) =
        IrradiationParams <$> v .: "equipmentname"
                          <*> v .: "lightsourcename"
                          <*> v .: "lightsourcechannel"
                          <*> v .: "lightsourcepower"
    parseJSON _ = fail "can't decode irradiation params"
instance ToJSON IrradiationParams where
    toEncoding (IrradiationParams eName lName channel power) =
        pairs ("equipmentname" .= eName <> "lightsourcename" .= lName
            <> "lightsourcechannel" .= channel <> "lightsourcepower" .= power)
    toJSON _ = error "no toJSON"

instance FromJSON RobotProgramExecutionParams where
    parseJSON (Object v) =
        RobotProgramExecutionParams <$> v .: "equipmentname"
                                    <*> v .: "robotname"
                                    <*> v .: "programname"
                                    <*> v .: "programarguments"
instance ToJSON RobotProgramExecutionParams where
    toJSON params =
        object [ "equipmentname"  .= rpepEquipmentName params
               , "robotname"      .= rpepRobotName params
               , "programname" .= rprpProgramName params
               , "programarguments" .= rprpProgramArguments params
               ]

instance FromJSON MovableComponentParams where
    parseJSON (Object v) =
        MovableComponentParams <$> v .: "equipmentname"
                               <*> v .: "movablecomponentsettings"
    parseJSON _ = fail "can't decode MovableComponentParams params"
instance ToJSON MovableComponentParams where
    toEncoding (MovableComponentParams eName settings) =
        pairs ("equipmentname" .= eName <> "movablecomponentsettings" .= settings)
    toJSON _ = error "no toJSON"

instance FromJSON PositionNameAndCoords where
    parseJSON (Object v) =
        PositionNameAndCoords <$> v .: "name"
                              <*> v .: "coordinates"
    parseJSON _ = fail "can't decode PositionNameAndCoords params"
instance ToJSON PositionNameAndCoords where
    toEncoding (PositionNameAndCoords name coords) =
        pairs ("name" .= name <> "coordinates" .= coords)
    toJSON _ = error "no toJSON"
instance MessagePack PositionNameAndCoords where
    toObject (PositionNameAndCoords name coords) =
        toObject $ M.fromList [("name" :: Text, toObject name),
                               ("coordinates", toObject coords)]

instance FromJSON RelativeStageLoopParams where
    parseJSON (Object v) =
        RelativeStageLoopParams <$> v .: "deltax"
                                <*> v .: "deltay"
                                <*> v .: "deltaz"
                                <*> v .: "additionalplanesx"
                                <*> v .: "additionalplanesy"
                                <*> v .: "additionalplanesz"
                                <*> v .: "returntostartingposition"
    parseJSON _ = fail "can't decode RelativeStageLoopParams"
instance ToJSON RelativeStageLoopParams where
    toEncoding (RelativeStageLoopParams dx dy dz px py pz ret) =
        pairs ("deltax" .= dx <> "deltay" .= dy <> "deltaz" .= dz <>
               "additionalplanesx" .= px <> "additionalplanesy" .= py <> "additionalplanesz" .= pz <> "returntostartingposition" .= ret)
    toJSON (RelativeStageLoopParams dx dy dz px py pz ret) = object
        [ "deltax" .= dx , "deltay" .= dy , "deltaz" .= dz,
          "additionalplanesx" .= px , "additionalplanesy" .= py, "additionalplanesz" .= pz,
          "returntostartingposition" .= ret
        ]

instance MessagePack RelativeStageLoopParams where
    toObject (RelativeStageLoopParams dx dy dz px py pz ret) =
        toObject $ M.fromList [("deltax" :: Text, toObject dx),
                               ("deltay" :: Text, toObject dy),
                               ("deltaz" :: Text, toObject dz),
                               ("additionalplanesx" :: Text, toObject px),
                               ("additionalplanesy" :: Text, toObject py),
                               ("additionalplanesz" :: Text, toObject pz),
                               ("returntostartingposition" :: Text, toObject ret)]

instance ToJSON AcquisitionTypeName where
    toJSON (AcquisitionTypeName n) = toJSON n
instance FromJSON AcquisitionTypeName where
    parseJSON = fmap AcquisitionTypeName . parseJSON
instance ToJSONKey AcquisitionTypeName where
    toJSONKey = toJSONKeyText fromAcqName
instance FromJSONKey AcquisitionTypeName where
    fromJSONKey = FromJSONKeyText AcquisitionTypeName

instance ToJSON WaitDuration where
    toJSON (WaitDuration n) = toJSON n
instance FromJSON WaitDuration where
    parseJSON = fmap WaitDuration . parseJSON
instance ToJSON NumIterationsTotal where
    toJSON (NumIterationsTotal n) = toJSON n
instance FromJSON NumIterationsTotal where
    parseJSON = fmap NumIterationsTotal . parseJSON 
instance ToJSON DetectionIndex where
    toJSON (DetectionIndex n) = toJSON n
instance FromJSON DetectionIndex where
    parseJSON = fmap DetectionIndex . parseJSON
instance ToJSON NumImagesInDetection where
    toJSON (NumImagesInDetection n) = toJSON n
instance FromJSON NumImagesInDetection where
    parseJSON = fmap NumImagesInDetection . parseJSON

instance MessagePack ChannelMessage where
    toObject m@(ChannelMessage{}) =
        toObject $ M.fromList [
                    ("index" :: Text, toObject (fromIntegral m.cmIdx :: Int)),
                    ("message", toObject m.cmMsg)]

instance ToJSON ChannelMessage

instance MessagePack AsyncMeasurementMessage where
    toObject m@(AcquiredDataMessage{}) =
        toObject $ M.fromList [
                    ("metadata" :: Text, toObject m.acquisitionMetaData),
                    ("data", toObject m.acquiredData),
                    ("type", toObject ("acquireddatamessage" :: Text))
                 ]
    toObject (SmartProgramDecisionMessage programID timestamp decision) =
        let jsonPayload = object [ "decision"  .= decision
                                 , "programid" .= programID
                                 , "timestamp" .= sseAsSeconds timestamp
                                 ]
        in toObject $ M.fromList [
                    ("payload" :: Text, toObject (T.decodeUtf8 . LB.toStrict . encode $ jsonPayload)),
                    ("type", toObject ("smartprogramdecisionmessage" :: Text))
                 ]

instance ToJSON AcquiredData where
    toJSON (AcquiredData nRows nCols (SecondsSinceStartOfExperiment timeStamp) camName bytes numType) =
        object ["nrows" .= nRows, "ncols" .= nCols, "timestamp" .= timeStamp, "detectorname" .= camName, "data" .= (show bytes), "numtype" .= (show numType)]
    toEncoding (AcquiredData nRows nCols (SecondsSinceStartOfExperiment timeStamp) camName bytes numType) =
        pairs ("nrows" .= nRows <> "ncols" .= nCols <> "timestamp" .= timeStamp <> "detectorname" .= camName <> "data" .= (show bytes) <> "numtype" .= (show numType))

instance MessagePack AcquiredData where
    toObject d = toObject $ M.fromList [
                             ("nrows" :: Text, toObject d.acqNRows),
                             ("ncols", toObject d.acqNCols),
                             ("timestamp", toObject (sseAsSeconds d.acqTimeStamp)),
                             ("detectorname", toObject d.acqDetectorName),
                             ("imagedata", toObject d.acqData),
                             ("numtype", toObject (encodedNumType d.acqNumType))
                          ]
    fromObject _ = error "no fromObject for AcquiredData"

instance ToJSON AcquisitionMetaData
instance FromJSON AcquisitionMetaData

instance MessagePack AcquisitionMetaData where
    toObject (AcquisitionMetaData position positionName typename detIdx nImagesInDetection) =
      toObject $ M.fromList [
                 ("stageposition" :: Text, toObject position),
                 ("stagepositionname", toObject positionName),
                 ("acquisitiontype", toObject (fromAcqName typename)),
                 ("detectionindex", toObject (fromDetectionIndex detIdx)),
                 ("nimageswithdetectionindex", toObject (fromNumImagesInDetection nImagesInDetection))
               ]
    fromObject _ = error "no fromObject for AcquisitionMetaData"

instance ToJSON AsyncMeasurementMessage where
  toJSON p@(AcquiredDataMessage{}) = toJSON (p.acquisitionMetaData, p.acquiredData) -- should not be used since this data should be binary encoded instead
  toJSON _                       = error "no ToJSON for this AcquiredDataMessage type"

instance ToJSON SmartProgramCode where
    toJSON (DAGOrchestratorCode v) =
        object ["type" .= ("dagorchestratorcode" :: Text), "code" .= v]
    toJSON (ProgramRunnerCode progs) =
        object ["type" .= ("programrunnercode" :: Text), "programs" .= progs]

instance FromJSON SmartProgramCode where
    parseJSON (Object v) =
        v .: "type" >>= \(t :: Text) ->
        case t of
            "dagorchestratorcode" -> DAGOrchestratorCode <$> v .: "code"
            "programrunnercode"   -> ProgramRunnerCode <$> v .: "programs"
            _                     -> error "failing to parse SmartProgramCode"

instance FromJSON SmartProgramID where
    parseJSON = fmap SmartProgramID . parseJSON
instance ToJSON SmartProgramID where
    toJSON (SmartProgramID i) = toJSON i

instance FromJSON LaunchableSmartProgram where
    parseJSON (Object v) =
        LaunchableSmartProgram <$> v .: "programid"
                               <*> v .: "programcode"
                               <*> v .: "pythoninterpreter"
                               <*> v .: "workingdirectory"

instance ToJSON LaunchableSmartProgram where
    toJSON (LaunchableSmartProgram programid programcode pythoninterpreter workingdirectory) =
        object [ "programid"         .= programid
               , "programcode"       .= programcode
               , "pythoninterpreter" .= pythoninterpreter
               , "workingdirectory"  .= workingdirectory
               ]

instance FromJSON SmartProgramServerResponse where
    parseJSON (Object v) = 
        v .: "type" >>= \(tt :: Text) ->
        case tt of
            "status"                    -> v .: "status" >>= \(st :: Text) -> 
                                               case st of
                                                   "success" -> pure ResponseSuccess
                                                   "error"   -> ResponseError <$> v .: "what"
            "nodecision"                -> ResponseNoDecision <$> v .: "comment"
            "dotimesdecision"           -> ResponseDoTimesDecision <$> v .: "ntotal" <*> v .: "comment"
            "stageloopdecision"         -> ResponseStageLoopDecision <$> v .: "positions" <*> v .: "comment"
            "relativestageloopdecision" -> ResponseRelativeStageLoopDecision <$> v .: "params" <*> v .: "comment"
            "timelapsedecision"         -> ResponseTimeLapseDecision <$> v .: "ntotal" <*> v .: "timedelta" <*> v .: "comment"
            _                           -> fail "unknown SmartProgramServerResponse"

instance ToJSON SmartProgramServerResponse where
    toJSON response = object $ case response of
        ResponseSuccess -> 
            [ "type"   .= ("status" :: Text)
            , "status" .= ("success" :: Text)]
        ResponseError what ->
            [ "type"   .= ("status" :: Text)
            , "status" .= ("error" :: Text)
            , "what"   .= what]
        ResponseNoDecision comment ->
            [ "type" .= ("nodecision" :: Text)
            , "comment" .= comment]
        ResponseDoTimesDecision ntotal comment ->
            [ "type"    .= ("dotimesdecision" :: Text)
            , "ntotal"  .= ntotal
            , "comment" .= comment]
        ResponseStageLoopDecision positions comment ->
            [ "type"      .= ("stageloopdecision" :: Text)
            , "positions" .= positions
            , "comment"   .= comment]
        ResponseRelativeStageLoopDecision params comment ->
            [ "type"    .= ("relativestageloopdecision" :: Text)
            , "params"  .= params
            , "comment" .= comment]
        ResponseTimeLapseDecision ntotal timedelta comment ->
            [ "type"      .= ("timelapsedecision" :: Text)
            , "ntotal"    .= ntotal
            , "timedelta" .= timedelta
            , "comment"   .= comment]