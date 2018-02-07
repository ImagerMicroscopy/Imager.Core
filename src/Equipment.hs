{-# LANGUAGE ExistentialQuantification #-}
module Equipment where

import Data.Text (Text)

data EquipmentW = forall e. (Equipment e) => EquipmentW e

type Name = Text
type FilterName = Text
type StagePosition = (Double, Double, Double)
type ChannelName = Text

data LightSourceDescription = LightSourceDescription {
                                  lsdName :: !Text
                                , lsdCanControlPower :: !Bool
                                , lsdAllowsMultipleChannels :: !Bool
                                , lsdChannels :: ![ChannelName]
                              }

class Equipment e where
    equipmentName :: e -> Text
    closeDevice :: e -> IO ()

    availableLightSources :: e -> [LightSourceDescription]
    activateLightSource :: e -> Name -> [(ChannelName, Double)] -> IO ()
    deactivateLightSource :: e -> IO ()

    availableFilterWheels  :: e -> [(Name, [FilterName])]
    switchToFilter :: e -> Name -> FilterName -> IO ()

    hasMotorizedStage :: e -> Bool
    motorizedStageName :: e -> Text
    getStagePosition :: e -> IO StagePosition
    setStagePosition :: e -> StagePosition -> IO ()

    hasRobot :: e -> Bool
    robotName :: e -> Text
    listRobotPrograms :: e -> IO [Text]
    robotAcceptsExternalCommands :: e -> IO Bool
    executeRobotProgram :: e -> Text -> Bool -> IO ()
    abortRobotProgramExecution :: e -> IO ()


    availableLightSources _ = []
    activateLightSource _ _ _ = error "calling activateLightSource"
    deactivateLightSource _ = error "calling deactivateLightSource"
    availableFilterWheels _ = []
    switchToFilter _ _ = error "calling switchToFilter"
    hasMotorizedStage _ = False
    motorizedStageName _  = error "calling motorizedStageName"
    getStagePosition _ = error "calling getStagePosition"
    setStagePosition _ _ = error "calling setStagePosition"
    hasRobot _ = False
    robotName _ = error "calling robotName"
    listRobotPrograms _ = error "calling listRobotPrograms"
    robotAcceptsExternalCommands _ = error "calling robotAcceptsExternalCommands"
    executeRobotProgram _ _ = error "calling executeRobotProgram"
    abortRobotProgramExecution _ = error "calling abortRobotProgramExecution"

instance Equipment EquipmentW where
    equipmentName (EquipmentW e) = equipmentName e
    closeDevice (EquipmentW e) = closeDevice e
    availableLightSources (EquipmentW e) = availableLightSources e
    activateLightSource (EquipmentW e) = activateLightSource e
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
