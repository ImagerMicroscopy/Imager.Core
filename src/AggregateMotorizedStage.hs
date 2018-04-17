module AggregateMotorizedStage
where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.List

import Equipment
import MiscUtils

{-
    overall idea: extract all equipment that has a motorized stage.
    If there is more than one then create an AggregateMotorizedStage
    object that will encapsulate these into an apparent single motorized stage.
    The original stage equipment is then wrapped in AllButMotorizedStageEquipment
    so that any non-stage functionality is still available.
-}

data AggregateMotorizedStage = AggregateMotorizedStage ![([StageAxis], EquipmentW)]

data AllButMotorizedStageEquipment = AllButMotorizedStageEquipment {
                                        abmsEq :: !EquipmentW
                                     }

instance Equipment AggregateMotorizedStage where
    equipmentName _ = EqName "AggregateStage"
    flushSerialPorts _ = pure ()
    closeDevice _ = pure ()
    hasMotorizedStage _ = True
    motorizedStageName _ = StageName "AggregateStage"
    supportedStageAxes (AggregateMotorizedStage as) = concat (map fst as)
    getStagePosition (AggregateMotorizedStage as) =
        let (axes, stages) = unzip as
        in  mapM getStagePosition stages >>= \allPositions ->
            pure (foldr f (-1.0, -1.0, -1.0) (zip axes allPositions))
        where
            f :: ([StageAxis], StagePosition) -> StagePosition -> StagePosition
            f (as, (x, y, z)) (accumX, accumY, accumZ) =
                let x' = if (XAxis `elem` as) then x else accumX
                    y' = if (YAxis `elem` as) then y else accumY
                    z' = if (ZAxis `elem` as) then z else accumZ
                in (x', y', z')
    setStagePosition (AggregateMotorizedStage as) pos =
        forConcurrently_ (map snd as) (\eq -> setStagePosition eq pos)

instance Equipment AllButMotorizedStageEquipment where
    equipmentName e = equipmentName (abmsEq e)
    flushSerialPorts e = flushSerialPorts (abmsEq e)
    closeDevice e = closeDevice (abmsEq e)
    availableLightSources e = availableLightSources (abmsEq e)
    activateLightSource e = activateLightSource (abmsEq e)
    activateLightSourceGated e = activateLightSourceGated (abmsEq e)
    deactivateLightSource e = deactivateLightSource (abmsEq e)
    availableFilterWheels e = availableFilterWheels (abmsEq e)
    switchToFilter e = switchToFilter (abmsEq e)

    hasMotorizedStage _ = False

    hasRobot e = hasRobot (abmsEq e)
    robotName e = robotName (abmsEq e)
    listRobotPrograms e = listRobotPrograms (abmsEq e)
    robotAcceptsExternalCommands e = robotAcceptsExternalCommands (abmsEq e)
    executeRobotProgram e = executeRobotProgram (abmsEq e)
    abortRobotProgramExecution e = abortRobotProgramExecution (abmsEq e)

aggregateMotorizedStagesIfNeeded :: [EquipmentW] -> [EquipmentW]
aggregateMotorizedStagesIfNeeded eqs
    | (length stages <= 1) = eqs
    | otherwise = let wrappedStages = map (EquipmentW . AllButMotorizedStageEquipment) stages
                      withoutStages = filter (not . hasMotorizedStage) eqs
                      zipped = map (\s -> (supportedStageAxes s, s)) stages
                      axes = concat (map fst zipped)
                  in  if (nodups axes)
                      then (EquipmentW (AggregateMotorizedStage zipped) : wrappedStages) ++ withoutStages
                      else throw (userError "ERROR: two or more stages can control the same axis")
    where
        stages = filter hasMotorizedStage eqs
