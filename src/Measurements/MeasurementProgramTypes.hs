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
import Data.Either
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Data.MessagePack
import Data.Monoid
import Data.Text (Text)
import Data.Word
import GHC.Generics
import System.Clock

import Camera.SCCameraTypes
import Equipment.Equipment
import Equipment.EquipmentTypes
import Camera.SCCameraTypes
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
                        | MEExecuteRobotProgram !RobotName !RobotProgramName !Bool
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
                              , peKnownSmartProgramIDs :: ![SmartProgramID]
                              , peSmartProgramCode :: SmartProgramCode
                              , peMessageChannel :: !MessageChannel
                              , peStatusMVar :: !(MVar [Text])
                              , peSmartProgramSendChan :: !(WaitableChannelWriter ([SmartProgramID], (AcquisitionMetaData, AcquiredData)))
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
          "executerobotprogram" -> MEExecuteRobotProgram <$> v .: "robotname" <*> v .: "programname" <*> v .: "waitforcompletion"
          "dotimes"     -> MEDoTimes <$> v .: "ntotal" <*> v .: "smartprogramids" <*> v .: "elements"
          "timelapse"   -> METimeLapse <$> v .: "ntotal" <*> v .: "timedelta" <*> v .: "smartprogramids" <*> v .: "elements"
          -- no FromJSON instance for MEFastAcquisitionLoop because it is automatically applied
          "stageloop"   -> MEStageLoop <$> v .: "stagename" <*> v .: "positions" <*> v .: "smartprogramids" <*> v .: "elements"
          "relativestageloop" -> MERelativeStageLoop <$> v .: "stagename" <*> v .: "params" <*> v .: "smartprogramids" <*> v .: "elements"
          _             -> fail "can't decode measurement element type"
    parseJSON _ = fail "can't decode measurement element"
instance ToJSON MeasurementElement where
  toEncoding (MEDetection dets programIDs) = pairs ("elementtype" .= ("detection" :: Text) <> "detectionnames" .= dets <> "smartprogramids" .= programIDs)
  toEncoding (MEIrradiation dur ip) = pairs ("elementtype" .= ("irradiation" :: Text) <> "duration" .= dur <> "irradiation" .= ip)
  toEncoding (MEWait d) = pairs ("elementtype" .= ("wait" :: Text) <> "duration" .= d)
  toEncoding (MEExecuteRobotProgram n p w) = pairs ("elementtype" .= ("executerobotprogram" :: Text) <> "robotname" .= n <> "programname" .= p <> "waitforcompletion" .= w)
  toEncoding (MEDoTimes n ids es) = pairs ("elementtype" .= ("dotimes" :: Text) <> "ntotal" .= n <> "smartprogramids" .= ids <> "elements" .= es)
  toEncoding (METimeLapse n td ids  es) = pairs ("elementtype" .= ("timelapse" :: Text) <> "ntotal" .= n <> "timedelta" .= td <> "smartprogramids" .= ids <>  "elements" .= es)
  toEncoding (MEFastAcquisitionLoop n det inputProgramID programIDs) = pairs ("elementtype" .= ("fastacquisitionloop" :: Text) <> "ntotal" .= n <> "detection" .= det <> "smartprogramid" .= inputProgramID <> "smartprogramids" .= programIDs)
  toEncoding (MEStageLoop n pos ids  es) = pairs ("elementtype" .= ("stageloop" :: Text) <> "stagename" .= n <> "positions" .= pos <> "smartprogramids" .= ids <> "elements" .= es)
  toEncoding (MERelativeStageLoop n ps ids  es) = pairs ("elementtype" .= ("relativestageloop" :: Text) <> "stagename" .= n <> "params" .= ps <> "smartprogramids" .= ids <> "elements" .= es)
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
    toJSON _ = error "no toJSON"

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
                             deriving (Show)

instance MessagePack AsyncMeasurementMessage where
    toObject m@(AcquiredDataMessage{}) =
        toObject $ M.fromList [
                    ("metadata" :: Text, toObject m.acquisitionMetaData),
                    ("data", toObject m.acquiredData),
                    ("type", toObject ("acquireddatamessage" :: Text))
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

measuredImageAsAcquiredData :: MeasuredImage -> DetectorName -> TimeAtStartOfExperiment -> TimeAtStartOfDetection -> AcquiredData
measuredImageAsAcquiredData (MeasuredImage nRows nCols (SecondsSinceStartOfDetection secsSinceDetStart) vecData) cameraName startOfExperiment startOfDetection =
    AcquiredData nRows nCols (SecondsSinceStartOfExperiment timeStamp) cameraName (byteStringFromVector vecData) UINT16
    where
        secondsBetweenStartOfDetAndStartOfExp = timeSpecAsSeconds (diffTimeSpec (tasdAsTimeSpec startOfDetection) (taseAsTimeSpec startOfExperiment))
        timeStamp = secondsBetweenStartOfDetAndStartOfExp + secsSinceDetStart

data AcquisitionMetaData = AcquisitionMetaData {
                               amdStagePosition :: !StagePosition
                             , amdAcquisitionTypename :: !AcquisitionTypeName
                             , amdDetectionIndex :: !DetectionIndex     -- The detection that this image logically belongs to.
                                                                        -- All images acquired in the same MEDetection will have the same detectionIndex.
                             , amdNImagesWithDetectionIndex :: !NumImagesInDetection     -- How many images there with this detectionIndex
                           } deriving (Show, Generic, NFData)

instance ToJSON AcquisitionMetaData
instance FromJSON AcquisitionMetaData

instance MessagePack AcquisitionMetaData where
    toObject (AcquisitionMetaData position typename detIdx nImagesInDetection) =
      toObject $ M.fromList [
                 ("stageposition" :: Text, toObject position),
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


newtype SmartProgramCode = SmartProgramCode {fromSmartProgramCode :: Value}
                              deriving (Show, Generic)
newtype SmartProgramID = SmartProgramID {fromSmartProgramID :: Text}
                        deriving (Show, Generic, Ord, Eq)

instance FromJSON SmartProgramCode where
    parseJSON = fmap SmartProgramCode . parseJSON
instance ToJSON SmartProgramCode where
    toJSON (SmartProgramCode c) = toJSON c
instance FromJSON SmartProgramID where
    parseJSON = fmap SmartProgramID . parseJSON
instance ToJSON SmartProgramID where
    toJSON (SmartProgramID i) = toJSON i


