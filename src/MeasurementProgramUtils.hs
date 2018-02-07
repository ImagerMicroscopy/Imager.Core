module MeasurementProgramUtils where

import Control.Exception
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Equipment

lookupMaybeLightSource :: [EquipmentW] -> (Name, Name) -> Maybe EquipmentW
lookupMaybeLightSource eqs (eqName, lsName) =
    case (filter (\e -> (equipmentName e == eqName) && (lsName `elem` (map lsdName $ availableLightSources e))) eqs) of
        [e] -> Just e
        _   -> Nothing


lookupLightSource :: [EquipmentW] -> (Name, Name) -> EquipmentW
lookupLightSource es n = fromJust $ lookupMaybeLightSource es n

lookupStageThrows :: [EquipmentW] -> Text -> EquipmentW
lookupStageThrows mss name = case eligibleStages of
                        [s] -> s
                        []  -> throw (userError ("no stage named " ++ (T.unpack name)))
                        _   -> throw (userError ("more than one stage with the same name"))
    where
      eligibleStages = filter (\e -> hasMotorizedStage e && (name == motorizedStageName e)) mss

lookupRobotThrows :: [EquipmentW] -> Text -> EquipmentW
lookupRobotThrows eqs rName = case (filter (\e -> hasRobot e && (robotName e == rName)) eqs) of
                                  [r] -> r
                                  _   -> throw (userError "missing robot in program validation")
