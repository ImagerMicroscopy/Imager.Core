{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables, DeriveGeneric, DeriveAnyClass #-}

module Measurements.MeasurementProgramTypes where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Either
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Data.MessagePack
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Generics
import System.Clock

import Camera.SCCameraTypes
import Encodings.EquipmentEncoding
import Equipment.Equipment
import Equipment.EquipmentTypes
import Utils.MiscUtils
import Utils.WaitableChannel

type Prog = [MeasurementElement]

newtype AcquisitionTypeName = AcquisitionTypeName {fromAcqName :: Text} 
                            deriving (Show, Eq, Generic, Ord, NFData)
newtype WaitDuration = WaitDuration {fromWaitDuration :: Double}
                            deriving (Show, Eq, Generic, Ord, NFData)
newtype NumIterationsTotal = NumIterationsTotal {fromNumIterationsTotal :: Int}
                            deriving (Show, Eq, Generic, Ord, NFData)
newtype DetectionIndex = DetectionIndex {fromDetectionIndex :: Int}
                            deriving (Show, Eq, Generic, Ord, NFData)
newtype NumImagesInDetection = NumImagesInDetection {fromNumImagesInDetection :: Int}
                            deriving (Show, Eq, Generic, Ord, NFData)

data MeasurementElement = MEDetection ![AcquisitionTypeName] ![SmartProgramID]
                        | MEIrradiation !LSIlluminationDuration ![IrradiationParams]
                        | MEWait !WaitDuration
                        | MEExecuteRobotProgram !RobotProgramExecutionParams
                        | MEDoTimes !NumIterationsTotal (Maybe SmartProgramID) !Prog
                        | MEFastAcquisitionLoop !NumIterationsTotal !(AcquisitionTypeName, DetectionParams) 
                                                !(Maybe SmartProgramID)     -- ask this smart program for parameters
                                                ![SmartProgramID]           -- send acquired images to these programs
                        | METimeLapse !NumIterationsTotal !WaitDuration !(Maybe SmartProgramID) !Prog
                        | MEStageLoop !StageName ![PositionNameAndCoords] !(Maybe SmartProgramID) !Prog
                        | MERelativeStageLoop !StageName !RelativeStageLoopParams !(Maybe SmartProgramID) !Prog
                        deriving (Show)

type DefinedDetections = Map AcquisitionTypeName DetectionParams

data ProgramEnvironment a = ProgramEnvironment {
                                peDetectors :: ![a]
                              , peStartTime :: !TimeAtStartOfExperiment
                              , peEquipment :: ![EquipmentW]
                              , peDetectionIndexRef :: !(IORef DetectionIndex)
                              , peCurrentNamedPosition :: !(IORef Text)   -- if we are doing a stage loop, name of the
                                                                          -- current position. Otherwise empty string.
                              , peKnownSmartProgramIDs :: ![SmartProgramID]
                              , peMessageChannel :: !MessageChannel
                              , peStatusMVar :: !(MVar [Text])
                              , peSmartProgramCommunicationFuncs :: !SmartProgramCommunicationFunctions
                              , peSmartProgramSendChan :: !(WaitableChannelWriter ([SmartProgramID], (AcquisitionMetaData, AcquiredData)))
                            }

data SmartProgramCommunicationFunctions = SmartProgramCommunicationFunctions {
                                              spcfSendImagesFunc :: [(AcquisitionMetaData, AcquiredData)] -> [SmartProgramID] -> IO ()
                                            , spcfGetSmartProgramDoTimesDecisionFunc :: SmartProgramID -> IO SmartProgramServerResponse
                                            , spcfGetSmartProgramStageLoopDecisionFunc :: SmartProgramID -> IO SmartProgramServerResponse
                                            , spcfGetSmartProgramRelativeStageLoopDecisionFunc :: SmartProgramID -> IO SmartProgramServerResponse
                                            , spcfGetSmartProgramTimeLapseDecisionFunc :: SmartProgramID -> IO SmartProgramServerResponse
                                          }

data DetectionParams = DetectionParams {
                           dpDetectors :: ![DetectorParams]
                         , dpIrradiation :: ![IrradiationParams]
                         , dpMovableComponents :: ![MovableComponentParams]
                       }
                       deriving (Show)

data DetectorParams = DetectorParams {
                          dtpDetectorName :: !DetectorName
                        , dtpDetectorProperties :: ![DetectorProperty]
                      }
                      deriving (Show)

data IrradiationParams = IrradiationParams {
                            ipEquipmentName :: !EqName
                          , ipLightSourceName :: !LSName
                          , ipLightSourceChannels :: ![LSChannelName]
                          , ipPowers :: ![LSIlluminationPower]
                        }
                        deriving (Show, Eq)

data MovableComponentParams = MovableComponentParams {
                                  mcEquipmentName :: !EqName
                                , mcComponentSettings :: ![MovableComponentSetting]
                              } deriving (Show, Eq)

data RobotProgramExecutionParams = RobotProgramExecutionParams {
                                       rpepEquipmentName :: !EqName
                                     , rpepRobotName :: !RobotName
                                     , rprpProgramName :: !RobotProgramName
                                     , rprpProgramArguments :: ![RobotProgramArgument]
                                   } deriving (Show)

data PositionNameAndCoords = PositionNameAndCoords !Text !StagePosition
                             deriving (Show)

data RelativeStageLoopParams = RelativeStageLoopParams {
                                   rslpDeltaX :: !Double
                                 , rslpDeltaY :: !Double
                                 , rslpDeltaZ :: !Double
                                 , rslpAdditionalPlanesX :: !(Int, Int)
                                 , rslpAdditionalPlanesY :: !(Int, Int)
                                 , rslpAdditionalPlanesZ :: !(Int, Int)
                                 , rslpReturnToStartingPosition :: !Bool
                               }
                               deriving (Show)

instance FromJSON MeasurementElement where
    parseJSON (Object v) =
        v .: "elementtype" >>= \(typ :: Text) ->
        case typ of
          "detection"   -> MEDetection <$> v .: "detectionnames" <*> v .: "smartprogramids"
          "irradiation" -> MEIrradiation <$> v .: "duration" <*> v .: "irradiation"
          "wait"        -> MEWait <$> v .: "duration"
          "executerobotprogram" -> MEExecuteRobotProgram <$> v .: "programparameters"
          "dotimes"     -> MEDoTimes <$> v .: "ntotal" <*> v .: "smartprogramid" <*> v .: "elements"
          "timelapse"   -> METimeLapse <$> v .: "ntotal" <*> v .: "timedelta" <*> v .: "smartprogramid" <*> v .: "elements"
          -- no FromJSON instance for MEFastAcquisitionLoop because it is automatically applied
          "stageloop"   -> MEStageLoop <$> v .: "stagename" <*> v .: "positions" <*> v .: "smartprogramid" <*> v .: "elements"
          "relativestageloop" -> MERelativeStageLoop <$> v .: "stagename" <*> v .: "params" <*> v .: "smartprogramid" <*> v .: "elements"
          _             -> fail "can't decode measurement element type"
    parseJSON _ = fail "can't decode measurement element"
instance ToJSON MeasurementElement where
  toEncoding (MEDetection dets programIDs) = pairs ("elementtype" .= ("detection" :: Text) <> "detectionnames" .= dets <> "smartprogramids" .= programIDs)
  toEncoding (MEIrradiation dur ip) = pairs ("elementtype" .= ("irradiation" :: Text) <> "duration" .= dur <> "irradiation" .= ip)
  toEncoding (MEWait d) = pairs ("elementtype" .= ("wait" :: Text) <> "duration" .= d)
  toEncoding (MEExecuteRobotProgram params) = pairs ("elementtype" .= ("executerobotprogram" :: Text) <> "programparameters" .= params)
  toEncoding (MEDoTimes n ids es) = pairs ("elementtype" .= ("dotimes" :: Text) <> "ntotal" .= n <> "smartprogramid" .= ids <> "elements" .= es)
  toEncoding (METimeLapse n td ids  es) = pairs ("elementtype" .= ("timelapse" :: Text) <> "ntotal" .= n <> "timedelta" .= td <> "smartprogramid" .= ids <>  "elements" .= es)
  toEncoding (MEFastAcquisitionLoop n det inputProgramID programIDs) = pairs ("elementtype" .= ("fastacquisitionloop" :: Text) <> "ntotal" .= n <> "detection" .= det <> "smartprogramid" .= inputProgramID <> "smartprogramids" .= programIDs)
  toEncoding (MEStageLoop n pos ids  es) = pairs ("elementtype" .= ("stageloop" :: Text) <> "stagename" .= n <> "positions" .= pos <> "smartprogramid" .= ids <> "elements" .= es)
  toEncoding (MERelativeStageLoop n ps ids  es) = pairs ("elementtype" .= ("relativestageloop" :: Text) <> "stagename" .= n <> "params" .= ps <> "smartprogramid" .= ids <> "elements" .= es)
  toJSON _ = error "no toJSON"

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

data ChannelMessage = ChannelMessage {
                          cmIdx :: !Word64,                 -- unique index of the message
                          cmMsg :: !AsyncMeasurementMessage -- message itself
                      }
                      deriving (Show, Generic)

instance MessagePack ChannelMessage where
    toObject m@(ChannelMessage{}) =
        toObject $ M.fromList [
                    ("index" :: Text, toObject (fromIntegral m.cmIdx :: Int)),
                    ("message", toObject m.cmMsg)]

instance ToJSON ChannelMessage

data MessageChannel = MessageChannel (MVar ([ChannelMessage], Word64))
    -- the channel contains messages and an incrementing Word64 counter that will yield the index of the next package to be received

newMessageChannel :: IO MessageChannel
newMessageChannel = MessageChannel <$> newMVar ([], 0)

sendMessageToChannel :: MessageChannel -> AsyncMeasurementMessage -> IO ()
sendMessageToChannel (MessageChannel mvar) msg =
    modifyMVar_ mvar (\(msgs, currentIdx) ->
        let nextIdx = currentIdx + 1
        in  pure (((ChannelMessage currentIdx msg) : msgs), nextIdx))

readChannelMessages :: MessageChannel -> IO [ChannelMessage]
readChannelMessages (MessageChannel mvar) = reverse . fst <$> readMVar mvar

deleteChannelMessagesUpToIndex :: MessageChannel -> Word64 -> IO ()
deleteChannelMessagesUpToIndex (MessageChannel mvar) idx =
    modifyMVar_ mvar (\(msgs, counter) ->
        pure $ (filter (\msg -> cmIdx msg > idx) msgs, counter))

numMessagesInChannel :: MessageChannel -> IO Int
numMessagesInChannel (MessageChannel mvar) =
    length . fst <$> readMVar mvar

data AsyncMeasurementMessage = AcquiredDataMessage {
                                   acquisitionMetaData :: AcquisitionMetaData
                                 , acquiredData :: AcquiredData
                               }
                             | SmartProgramDecisionMessage {
                                   spdmProgramID :: !SmartProgramID
                                 , spdmTimeStamp :: !SecondsSinceStartOfExperiment
                                 , spdmDecision :: !SmartProgramServerResponse
                               }
                             deriving (Show)

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

data AcquiredData = AcquiredData {
                        acqNRows :: !Int
                      , acqNCols :: !Int
                      , acqTimeStamp :: !SecondsSinceStartOfExperiment
                      , acqDetectorName :: !DetectorName
                      , acqData :: !ByteString
                      , acqNumType :: !NumberType
                  } deriving (Show, Generic, NFData)

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

measuredImageAsAcquiredData :: MeasuredImage -> DetectorName -> TimeAtStartOfExperiment -> TimeAtStartOfEvent -> AcquiredData
measuredImageAsAcquiredData (MeasuredImage nRows nCols (SecondsSinceStartOfDetection secsSinceDetStart) vecData) cameraName startOfExperiment startOfDetection =
    AcquiredData nRows nCols (SecondsSinceStartOfExperiment timeStamp) cameraName (byteStringFromVector vecData) UINT16
    where
        secondsBetweenStartOfDetAndStartOfExp = timeSpecAsSeconds (diffTimeSpec (taseAsTimeSpec startOfDetection) (tasexAsTimeSpec startOfExperiment))
        timeStamp = secondsBetweenStartOfDetAndStartOfExp + secsSinceDetStart

smartProgramDecisionAsMessage :: SmartProgramServerResponse -> SmartProgramID -> TimeAtStartOfExperiment -> TimeAtStartOfEvent -> AsyncMeasurementMessage
smartProgramDecisionAsMessage response programID timeAtExpStart timeAtDecision =
    SmartProgramDecisionMessage programID (SecondsSinceStartOfExperiment timestamp) response
    where
        timestamp = timeSpecAsSeconds (diffTimeSpec (taseAsTimeSpec timeAtDecision) (tasexAsTimeSpec timeAtExpStart))

data AcquisitionMetaData = AcquisitionMetaData {
                               amdStagePosition :: !StagePosition
                             , amdStagePositionName :: !Text
                             , amdAcquisitionTypename :: !AcquisitionTypeName
                             , amdDetectionIndex :: !DetectionIndex     -- The detection that this image logically belongs to.
                                                                        -- All images acquired in the same MEDetection will have the same detectionIndex.
                             , amdNImagesWithDetectionIndex :: !NumImagesInDetection     -- How many images there with this detectionIndex
                           } deriving (Show, Generic, NFData)

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

data NumberType = UINT8
                | UINT16
                | FP64
                deriving (Show, Eq)

encodedNumType :: NumberType -> Int
encodedNumType UINT8 = 2
encodedNumType UINT16 = 0
encodedNumType FP64 = 1

instance NFData NumberType where
  rnf t = t `seq` ()
instance NFData TimeSpec where
  rnf t = t `seq` ()

data SmartProgramCode = DAGOrchestratorCode {
                            fromDAGOrchestratorCode :: !Value
                        }
                      | ProgramRunnerCode {
                            fromProgramRunnerCode :: ![LaunchableSmartProgram]
                        }
                        deriving (Show)
newtype SmartProgramID = SmartProgramID {fromSmartProgramID :: Text}
                        deriving (Show, Ord, Eq)

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

type Port = Int

data LaunchableSmartProgram = LaunchableSmartProgram {
                                  lspID :: !SmartProgramID
                                , lspCode :: !Text
                                , lspPythonInterpreter :: !FilePath
                                , lspWorkingDirectory :: !FilePath
                              } deriving (Show)

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


data SmartProgramServerResponse = ResponseSuccess
                                | ResponseError !Text
                                | ResponseNoDecision
                                | ResponseDoTimesDecision !NumIterationsTotal !Text
                                | ResponseStageLoopDecision ![PositionNameAndCoords] !Text
                                | ResponseRelativeStageLoopDecision !RelativeStageLoopParams !Text
                                | ResponseTimeLapseDecision !NumIterationsTotal !WaitDuration !Text
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
        ResponseNoDecision ->
            [ "type" .= ("nodecision" :: Text) ]
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

type SendToSmartProgramsChannelWriter = WaitableChannelWriter ([SmartProgramID], (AcquisitionMetaData, AcquiredData))
type SendToSmartProgramsChannelReader = WaitableChannelReader ([SmartProgramID], (AcquisitionMetaData, AcquiredData))
