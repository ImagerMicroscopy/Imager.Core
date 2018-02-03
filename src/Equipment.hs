{-# LANGUAGE ExistentialQuantification #-}
module Equipment where

import Data.Text (Text)

data EquipmentW = forall e. (Equipment e) => EquipmentW e

type StagePosition = (Double, Double, Double)
type ChannelName = Text

class Equipment e where
    equipmentName :: e -> Text
    closeDevice :: e -> IO ()

    hasLightSource :: e -> Bool
    lightSourceName :: e -> Text
    lightSourceCanControlPower :: e -> Bool
    lightSourceAllowsMultipleChannels :: e -> Bool
    lightSourceChannels :: e -> [ChannelName]
    activateLightSource :: e -> [(ChannelName, Double)] -> IO ()
    deactivateLightSource :: e -> IO ()

    hasFilterWheel  :: e -> Bool
    filterWheelName :: e -> Text
    filterWheelChannels :: e -> [Text]
    switchToFilter :: e -> Text -> IO ()

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


    hasLightSource _ = False
    lightSourceName _ = error "calling lightSourceName"
    lightSourceCanControlPower _ = error "calling lightSourceCanControlPower"
    lightSourceAllowsMultipleChannels _ = error "calling lightSourceAllowsMultipleChannels"
    lightSourceChannels _ = error "calling lightSourceChannels"
    activateLightSource _ _ = error "calling activateLightSource"
    deactivateLightSource _ = error "calling deactivateLightSource"
    hasFilterWheel _ = False
    filterWheelName _ = error "calling filterWheelName"
    filterWheelChannels _ = error "calling filterWheelChannels"
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
    hasLightSource (EquipmentW e) = hasLightSource e
    lightSourceName (EquipmentW e) = lightSourceName e
    lightSourceCanControlPower (EquipmentW e) = lightSourceCanControlPower e
    lightSourceAllowsMultipleChannels (EquipmentW e) = lightSourceAllowsMultipleChannels e
    lightSourceChannels (EquipmentW e) = lightSourceChannels e
    activateLightSource (EquipmentW e) = activateLightSource e
    deactivateLightSource (EquipmentW e) = deactivateLightSource e
    hasFilterWheel (EquipmentW e) = hasFilterWheel e
    filterWheelName (EquipmentW e) = filterWheelName e
    filterWheelChannels (EquipmentW e) = filterWheelChannels e
    switchToFilter (EquipmentW e) = switchToFilter e
    hasMotorizedStage (EquipmentW e) = hasMotorizedStage e
    motorizedStageName (EquipmentW e)  = motorizedStageName e
    getStagePosition (EquipmentW e) = getStagePosition e
    setStagePosition (EquipmentW e) = setStagePosition e
    hasRobot (EquipmentW e) = hasRobot e
    robotName (EquipmentW e) = robotName e
    listRobotPrograms (EquipmentW e) = listRobotPrograms e
    executeRobotProgram (EquipmentW e) = executeRobotProgram e
