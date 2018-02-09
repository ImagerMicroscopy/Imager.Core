{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module Equipment where

import Data.Aeson
import Data.Text (Text)

data EquipmentW = forall e. (Equipment e) => EquipmentW e

newtype EqName = EqName {
                     fromEqName :: Text
                 } deriving (Show, Eq, Ord, Monoid, ToJSON, FromJSON)
newtype LSName = LSName {
                     fromLSName :: Text
                 } deriving (Show, Eq, Ord, Monoid, ToJSON, FromJSON)
newtype LSChannelName = LSChannelName {
                            fromLSChannelName :: Text
                        } deriving (Show, Eq, Ord, Monoid, ToJSON, FromJSON)
newtype LSIlluminationPower = LSIlluminationPower {
                                  fromLSIlluminationPower :: Double
                              } deriving (Show, Eq, Ord, ToJSON, FromJSON)
newtype FWName = FWName {
                     fromFWName :: Text
                 } deriving (Show, Eq, Ord, ToJSON, FromJSON)
newtype FName = FName {
                   fromFName :: Text
                } deriving (Show, Eq, Ord, ToJSON, FromJSON)
newtype StageName = StageName {
                        fromStageName :: Text
                    } deriving (Show, Eq, Ord, ToJSON, FromJSON)
newtype RobotName = RobotName {
                        fromRobotName :: Text
                    } deriving (Show, Eq, Ord, ToJSON, FromJSON)
newtype RobotProgramName = RobotProgramName {
                               fromRobotProgramName :: Text
                           } deriving (Show, Eq, Ord, ToJSON, FromJSON)

type StagePosition = (Double, Double, Double)

data LightSourceDescription = LightSourceDescription {
                                  lsdName :: !LSName
                                , lsdCanControlPower :: !Bool
                                , lsdAllowsMultipleChannels :: !Bool
                                , lsdChannels :: ![LSChannelName]
                              }
data FilterWheelDescription = FilterWheelDescription {
                                  fwdName :: !FWName
                                , fwdFilters :: ![FName]
                              }

class Equipment e where
    equipmentName :: e -> EqName
    closeDevice :: e -> IO ()

    availableLightSources :: e -> [LightSourceDescription]
    activateLightSource :: e -> LSName -> [(LSChannelName, LSIlluminationPower)] -> IO ()
    activateLightSourceGated :: e -> LSName -> [(LSChannelName, LSIlluminationPower)] -> IO ()
    deactivateLightSource :: e -> IO ()

    availableFilterWheels  :: e -> [FilterWheelDescription]
    switchToFilter :: e -> FWName -> FName -> IO ()

    hasMotorizedStage :: e -> Bool
    motorizedStageName :: e -> StageName
    getStagePosition :: e -> IO StagePosition
    setStagePosition :: e -> StagePosition -> IO ()

    hasRobot :: e -> Bool
    robotName :: e -> RobotName
    listRobotPrograms :: e -> IO [RobotProgramName]
    robotAcceptsExternalCommands :: e -> IO Bool
    executeRobotProgram :: e -> RobotProgramName -> Bool -> IO ()
    abortRobotProgramExecution :: e -> IO ()


    availableLightSources _ = []
    activateLightSource e _ _ = error ("calling activateLightSource on " ++ show (fromEqName (equipmentName e)))
    activateLightSourceGated = activateLightSource
    deactivateLightSource e = error ("calling deactivateLightSource on " ++ show (fromEqName (equipmentName e)))
    availableFilterWheels _ = []
    switchToFilter e _ = error ("calling switchToFilter on " ++ show (fromEqName (equipmentName e)))
    hasMotorizedStage _ = False
    motorizedStageName e  = error ("calling motorizedStageName on " ++ show (fromEqName (equipmentName e)))
    getStagePosition e = error ("calling getStagePosition on " ++ show (fromEqName (equipmentName e)))
    setStagePosition e _ = error ("calling setStagePosition on " ++ show (fromEqName (equipmentName e)))
    hasRobot e = False
    robotName e = error ("calling robotName on " ++ show (fromEqName (equipmentName e)))
    listRobotPrograms e = error ("calling listRobotPrograms on " ++ show (fromEqName (equipmentName e)))
    robotAcceptsExternalCommands e = error ("calling robotAcceptsExternalCommands on " ++ show (fromEqName (equipmentName e)))
    executeRobotProgram e _ = error ("calling executeRobotProgram on " ++ show (fromEqName (equipmentName e)))
    abortRobotProgramExecution e = error ("calling abortRobotProgramExecution on " ++ show (fromEqName (equipmentName e)))

instance Equipment EquipmentW where
    equipmentName (EquipmentW e) = equipmentName e
    closeDevice (EquipmentW e) = closeDevice e
    availableLightSources (EquipmentW e) = availableLightSources e
    activateLightSource (EquipmentW e) = activateLightSource e
    activateLightSourceGated (EquipmentW e) = activateLightSourceGated e
    deactivateLightSource (EquipmentW e) = deactivateLightSource e
    availableFilterWheels (EquipmentW e) = availableFilterWheels e
    switchToFilter (EquipmentW e) = switchToFilter e
    hasMotorizedStage (EquipmentW e) = hasMotorizedStage e
    motorizedStageName (EquipmentW e)  = motorizedStageName e
    getStagePosition (EquipmentW e) = getStagePosition e
    setStagePosition (EquipmentW e) = setStagePosition e
    hasRobot (EquipmentW e) = hasRobot e
    robotName (EquipmentW e) = robotName e
    listRobotPrograms (EquipmentW e) = listRobotPrograms e
    executeRobotProgram (EquipmentW e) = executeRobotProgram e
