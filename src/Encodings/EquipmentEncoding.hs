{-# LANGUAGE OverloadedStrings #-}

module Encodings.EquipmentEncoding
where

import Data.Aeson
import qualified Data.Text as T

import Equipment.Equipment
import Equipment.EquipmentTypes

instance ToJSON LightSourceDescription where
    toJSON l = object ["name" .= lsdName l, "cancontrolpower" .= lsdCanControlPower l,
                       "allowmultiplechannels" .= lsdAllowsMultipleChannels l,
                       "channels" .= lsdChannels l]

instance ToJSON FilterWheelDescription where
    toJSON fw = object ["name" .= fwdName fw, "filters" .= fwdFilters fw]

instance ToJSON EquipmentW where
    toJSON e = object ["name" .= equipmentName e,
                       "availablelightsources" .= availableLightSources e,
                       "availablemovablecomponents" .= availableMovableComponents e,
                       "hasmotorizedstage" .= hasMotorizedStage e,
                       "motorizedstageName" .= stageN,
                       "hasrobot" .= hasRobot e,
                       "robotname" .= robotN]
       where
           stageN = if (hasMotorizedStage e) then (fromStageName (motorizedStageName e)) else T.empty
           robotN = if (hasRobot e) then (fromRobotName (robotName e)) else T.empty
