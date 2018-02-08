module MeasurementProgramUtils where

import Control.Exception
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Equipment

lookupMaybeLightSource :: [EquipmentW] -> (EqName, LSName) -> Maybe EquipmentW
lookupMaybeLightSource eqs (eqName, lsName) =
    case (filter (\e -> (equipmentName e == eqName) && (lsName `elem` (map lsdName $ availableLightSources e))) eqs) of
        [e] -> Just e
        _   -> Nothing


lookupLightSource :: [EquipmentW] -> (EqName, LSName) -> EquipmentW
lookupLightSource es n = fromJust $ lookupMaybeLightSource es n

lookupStageThrows :: [EquipmentW] -> StageName -> EquipmentW
lookupStageThrows eqs name = case eligibleStages of
                        [s] -> s
                        []  -> throw (userError ("no stage named " ++ (T.unpack $ fromStageName name)))
                        _   -> throw (userError ("more than one stage with the same name"))
    where
      eligibleStages = filter (\e -> hasMotorizedStage e && (name == motorizedStageName e)) eqs

lookupRobotThrows :: [EquipmentW] -> RobotName -> EquipmentW
lookupRobotThrows eqs eqName = case (filter (\e -> hasRobot e && (robotName e == eqName)) eqs) of
                                   [r] -> r
                                   _   -> throw (userError "missing robot in program validation")
