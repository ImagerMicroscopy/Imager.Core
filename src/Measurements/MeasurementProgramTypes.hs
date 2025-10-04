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

data ChannelMessage = ChannelMessage {
                          cmIdx :: !Word64,                 -- unique index of the message
                          cmMsg :: !AsyncMeasurementMessage -- message itself
                      }
                      deriving (Show, Generic)



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



data AcquiredData = AcquiredData {
                        acqNRows :: !Int
                      , acqNCols :: !Int
                      , acqTimeStamp :: !SecondsSinceStartOfExperiment
                      , acqDetectorName :: !DetectorName
                      , acqData :: !ByteString
                      , acqNumType :: !NumberType
                  } deriving (Show, Generic, NFData)



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



type Port = Int

data LaunchableSmartProgram = LaunchableSmartProgram {
                                  lspID :: !SmartProgramID
                                , lspCode :: !Text
                                , lspPythonInterpreter :: !FilePath
                                , lspWorkingDirectory :: !FilePath
                              } deriving (Show)




data SmartProgramServerResponse = ResponseSuccess
                                | ResponseError !Text
                                | ResponseNoDecision !Text
                                | ResponseDoTimesDecision !NumIterationsTotal !Text
                                | ResponseStageLoopDecision ![PositionNameAndCoords] !Text
                                | ResponseRelativeStageLoopDecision !RelativeStageLoopParams !Text
                                | ResponseTimeLapseDecision !NumIterationsTotal !WaitDuration !Text
                                deriving (Show)

isNoDecisionResponse :: SmartProgramServerResponse -> Bool
isNoDecisionResponse ResponseNoDecision{} = True
isNoDecisionResponse _                    = False

isSuccessResponse :: SmartProgramServerResponse -> Bool
isSuccessResponse ResponseSuccess = True
isSuccessResponse _               = False



type SendToSmartProgramsChannelWriter = WaitableChannelWriter ([SmartProgramID], (AcquisitionMetaData, AcquiredData))
type SendToSmartProgramsChannelReader = WaitableChannelReader ([SmartProgramID], (AcquisitionMetaData, AcquiredData))
