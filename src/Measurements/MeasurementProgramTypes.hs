{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}

module Measurements.MeasurementProgramTypes where

import Control.Concurrent
import Data.Aeson
import Data.Either
import Data.IORef
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import Data.Word
import System.Clock

import AcquiredDataTypes
import Detectors.Detector
import Equipment.Equipment
import Equipment.EquipmentTypes
import Camera.SCCameraTypes

type Prog = [MeasurementElement]

data MeasurementElement = MEDetection ![Text]
                        | MEIrradiation !LSIlluminationDuration ![IrradiationParams]
                        | MEWait !Double
                        | MEExecuteRobotProgram !RobotName !RobotProgramName !Bool
                        | MEDoTimes !Int !Prog
                        | MEFastAcquisitionLoop !Int !(Text, DetectionParams)
                        | METimeLapse !Int !Double !Prog
                        | MEStageLoop !StageName ![PositionNameAndCoords] !Prog
                        | MERelativeStageLoop !StageName !RelativeStageLoopParams !Prog
                        deriving (Show)

type DefinedDetections = Map Text DetectionParams

data ProgramEnvironment a = ProgramEnvironment {
                                peDetectors :: ![a]
                              , peStartTime :: !TimeSpec
                              , peEquipment :: ![EquipmentW]
                              , peDataCounter :: !(IORef Word64)
                              , peDataMVar :: !(MVar [AsyncMeasurementMessage])
                              , peStatusMVar :: !(MVar [Text])
                            }

data DetectionParams = DetectionParams {
                           dpDetectors :: ![DetectorParams]
                         , dpIrradiation :: ![IrradiationParams]
                         , dpMovableComponents :: ![MovableComponentParams]
                       }
                       deriving (Show)

data DetectorParams = DetectorParams {
                          dtpDetectorName :: !Text
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
          "detection"   -> MEDetection <$> v .: "detectionnames"
          "irradiation" -> MEIrradiation <$> v .: "duration" <*> v .: "irradiation"
          "wait"        -> MEWait <$> v .: "duration"
          "executerobotprogram" -> MEExecuteRobotProgram <$> v .: "robotname" <*> v .: "programname" <*> v .: "waitforcompletion"
          "dotimes"     -> MEDoTimes <$> v .: "ntotal" <*> v .: "elements"
          "timelapse"   -> METimeLapse <$> v .: "ntotal" <*> v .: "timedelta" <*> v .: "elements"
          -- no FromJSON instance for MEFastAcquisitionLoop because it is automatically applied
          "stageloop"   -> MEStageLoop <$> v .: "stagename" <*> v .: "positions" <*> v .: "elements"
          "relativestageloop" -> MERelativeStageLoop <$> v .: "stagename" <*> v .: "params" <*> v .: "elements"
          _             -> fail "can't decode measurement element type"
    parseJSON _ = fail "can't decode measurement element"
instance ToJSON MeasurementElement where
  toEncoding (MEDetection dets) = pairs ("elementtype" .= ("detection" :: Text) <> "detectionnames" .= dets)
  toEncoding (MEIrradiation dur ip) = pairs ("elementtype" .= ("irradiation" :: Text) <> "duration" .= dur <> "irradiation" .= ip)
  toEncoding (MEWait d) = pairs ("elementtype" .= ("wait" :: Text) <> "duration" .= d)
  toEncoding (MEExecuteRobotProgram n p w) = pairs ("elementtype" .= ("executerobotprogram" :: Text) <> "robotname" .= n <> "programname" .= p <> "waitforcompletion" .= w)
  toEncoding (MEDoTimes n es) = pairs ("elementtype" .= ("dotimes" :: Text) <> "ntotal" .= n <> "elements" .= es)
  toEncoding (METimeLapse n td es) = pairs ("elementtype" .= ("timelapse" :: Text) <> "ntotal" .= n <> "timedelta" .= td <> "elements" .= es)
  toEncoding (MEFastAcquisitionLoop n det) = pairs ("elementtype" .= ("fastacquisitionloop" :: Text) <> "ntotal" .= n <> "detection" .= det)
  toEncoding (MEStageLoop n pos es) = pairs ("elementtype" .= ("stageloop" :: Text) <> "stagename" .= n <> "positions" .= pos <> "elements" .= es)
  toEncoding (MERelativeStageLoop n ps es) = pairs ("elementtype" .= ("relativestageloop" :: Text) <> "stagename" .= n <> "params" .= ps <> "elements" .= es)
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
