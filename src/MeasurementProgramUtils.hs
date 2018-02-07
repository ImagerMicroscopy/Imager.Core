module MeasurementProgramUtils where

import Control.Exception
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Equipment

lookupMaybeLightSource :: [EquipmentW] -> (EqName, Name) -> Maybe EquipmentW
lookupMaybeLightSource eqs (eqName, lsName) =
    case (filter (\e -> (equipmentName e == eqName) && (lsName `elem` (map lsdName $ availableLightSources e))) eqs) of
        [e] -> Just e
        _   -> Nothing


lookupLightSource :: [EquipmentW] -> (EqName, Name) -> EquipmentW
lookupLightSource es n = fromJust $ lookupMaybeLightSource es n

lookupStageThrows :: [EquipmentW] -> EqName -> EquipmentW
lookupStageThrows eqs name = case eligibleStages of
                        [s] -> s
                        []  -> throw (userError ("no stage named " ++ (T.unpack $ fromEqName name)))
                        _   -> throw (userError ("more than one stage with the same name"))
    where
      eligibleStages = filter (\e -> hasMotorizedStage e && (name == equipmentName e)) eqs

lookupRobotThrows :: [EquipmentW] -> EqName -> EquipmentW
lookupRobotThrows eqs eqName = case (filter (\e -> hasRobot e && (equipmentName e == eqName)) eqs) of
                                   [r] -> r
                                   _   -> throw (userError "missing robot in program validation")
