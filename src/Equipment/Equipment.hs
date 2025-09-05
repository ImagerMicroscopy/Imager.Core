{-# LANGUAGE ExistentialQuantification #-}

module Equipment.Equipment where

import Control.Concurrent
import Data.Text (Text)

import Equipment.EquipmentTypes

data EquipmentW = forall e. (Equipment e) => EquipmentW e

class Equipment e where
    equipmentName :: e -> EqName
    flushSerialPorts :: e -> IO ()
    closeDevice :: e -> IO ()

    availableLightSources :: e -> [LightSourceDescription]
    activateLightSource :: e -> LSName -> [(LSChannelName, LSIlluminationPower)] -> IO ()
    activateLightSourceTimed :: e -> LSName -> [(LSChannelName, LSIlluminationPower)] -> LSIlluminationDuration -> IO ()
    deactivateLightSource :: e -> IO ()

    availableMovableComponents :: e -> [MovableComponentDescription]
    moveComponent :: e -> [MovableComponentSetting] -> IO ()

    hasMotorizedStage :: e -> Bool
    motorizedStageName :: e -> StageName
    supportedStageAxes :: e -> [StageAxis]
    getStagePosition :: e -> IO StagePosition
    setStagePosition :: e -> StagePosition -> IO ()

    availableRobots :: e -> [RobotDescription]
    executeRobotProgram :: e -> RobotName -> RobotProgramName -> [RobotProgramArgument] -> IO ()
    abortRobotProgramExecution :: e -> IO ()

    availableLightSources _ = []
    activateLightSource e _ _ = error ("calling activateLightSource on " ++ show (fromEqName (equipmentName e)))
    activateLightSourceTimed e n chs dur = activateLightSource e n chs >>
                                           threadDelay (round ((fromLSIlluminationDuration dur) * 1.0e6)) >>
                                           deactivateLightSource e
    deactivateLightSource e = error ("calling deactivateLightSource on " ++ show (fromEqName (equipmentName e)))

    availableMovableComponents _ = []
    moveComponent e = error ("calling moveComponent on " ++ show (fromEqName (equipmentName e)))

    hasMotorizedStage _ = False
    motorizedStageName e  = error ("calling motorizedStageName on " ++ show (fromEqName (equipmentName e)))
    supportedStageAxes e = error ("calling supportedStageAxes on " ++ show (fromEqName (equipmentName e)))
    getStagePosition e = error ("calling getStagePosition on " ++ show (fromEqName (equipmentName e)))
    setStagePosition e _ = error ("calling setStagePosition on " ++ show (fromEqName (equipmentName e)))
    
    availableRobots e = []
    executeRobotProgram e _ _ _ = error ("calling default executeRobotProgram implementation on "  ++ show (fromEqName (equipmentName e)))
    abortRobotProgramExecution e = error ("calling default abortRobotProgramExecution on " ++ show (fromEqName (equipmentName e)))

instance Equipment EquipmentW where
    equipmentName (EquipmentW e) = equipmentName e
    flushSerialPorts (EquipmentW e) = flushSerialPorts e
    closeDevice (EquipmentW e) = closeDevice e
    availableLightSources (EquipmentW e) = availableLightSources e
    activateLightSource (EquipmentW e) = activateLightSource e
    activateLightSourceTimed (EquipmentW e) = activateLightSourceTimed e
    deactivateLightSource (EquipmentW e) = deactivateLightSource e
    availableMovableComponents (EquipmentW e) = availableMovableComponents e
    moveComponent (EquipmentW e) = moveComponent e
    hasMotorizedStage (EquipmentW e) = hasMotorizedStage e
    motorizedStageName (EquipmentW e)  = motorizedStageName e
    supportedStageAxes (EquipmentW e) = supportedStageAxes e
    getStagePosition (EquipmentW e) = getStagePosition e
    setStagePosition (EquipmentW e) = setStagePosition e
    availableRobots (EquipmentW e) = availableRobots e
    executeRobotProgram (EquipmentW e) = executeRobotProgram e
    abortRobotProgramExecution (EquipmentW e) = abortRobotProgramExecution e
