{-# LANGUAGE ExistentialQuantification #-}
module Equipment where

import Control.Concurrent
import Data.Text (Text)

import EquipmentTypes

data EquipmentW = forall e. (Equipment e) => EquipmentW e

class Equipment e where
    equipmentName :: e -> EqName
    flushSerialPorts :: e -> IO ()
    closeDevice :: e -> IO ()

    availableLightSources :: e -> [LightSourceDescription]
    activateLightSource :: e -> LSName -> [(LSChannelName, LSIlluminationPower)] -> IO ()
    activateLightSourceTimed :: e -> LSName -> [(LSChannelName, LSIlluminationPower)] -> LSIlluminationDuration -> IO ()
    deactivateLightSource :: e -> IO ()

    availableFilterWheels  :: e -> [FilterWheelDescription]
    switchToFilter :: e -> FWName -> FName -> IO ()

    hasMotorizedStage :: e -> Bool
    motorizedStageName :: e -> StageName
    supportedStageAxes :: e -> [StageAxis]
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
    activateLightSourceTimed e n chs dur = activateLightSource e n chs >>
                                           threadDelay (round ((fromLSIlluminationDuration dur) * 1.0e6)) >>
                                           deactivateLightSource e
    deactivateLightSource e = error ("calling deactivateLightSource on " ++ show (fromEqName (equipmentName e)))
    availableFilterWheels _ = []
    switchToFilter e _ = error ("calling switchToFilter on " ++ show (fromEqName (equipmentName e)))
    hasMotorizedStage _ = False
    motorizedStageName e  = error ("calling motorizedStageName on " ++ show (fromEqName (equipmentName e)))
    supportedStageAxes e = error ("calling supportedStageAxes on " ++ show (fromEqName (equipmentName e)))
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
    flushSerialPorts (EquipmentW e) = flushSerialPorts e
    closeDevice (EquipmentW e) = closeDevice e
    availableLightSources (EquipmentW e) = availableLightSources e
    activateLightSource (EquipmentW e) = activateLightSource e
    activateLightSourceTimed (EquipmentW e) = activateLightSourceTimed e
    deactivateLightSource (EquipmentW e) = deactivateLightSource e
    availableFilterWheels (EquipmentW e) = availableFilterWheels e
    switchToFilter (EquipmentW e) = switchToFilter e
    hasMotorizedStage (EquipmentW e) = hasMotorizedStage e
    motorizedStageName (EquipmentW e)  = motorizedStageName e
    supportedStageAxes (EquipmentW e) = supportedStageAxes e
    getStagePosition (EquipmentW e) = getStagePosition e
    setStagePosition (EquipmentW e) = setStagePosition e
    hasRobot (EquipmentW e) = hasRobot e
    robotName (EquipmentW e) = robotName e
    listRobotPrograms (EquipmentW e) = listRobotPrograms e
    robotAcceptsExternalCommands (EquipmentW e) = robotAcceptsExternalCommands e
    executeRobotProgram (EquipmentW e) = executeRobotProgram e
    abortRobotProgramExecution (EquipmentW e) = abortRobotProgramExecution e
