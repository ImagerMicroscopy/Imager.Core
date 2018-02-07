{-# LANGUAGE OverloadedStrings #-}

module EquipmentEncoding
where

import Data.Aeson

import Equipment
import EquipmentTypes

instance ToJSON LightSourceDescription where
    toJSON l = object ["name" .= lsdName l, "cancontrolpower" .= lsdCanControlPower l,
                       "allowmultiplechannels" .= lsdAllowsMultipleChannels l,
                       "channels" .= lsdChannels l]

instance ToJSON EquipmentW where
    toJSON e = object ["name" .= equipmentName e,
                       "availablelightsources" .= availableLightSources e,
                       "availableFilterWheels" .= availableFilterWheels e,
                       "hasmotorizedstage" .= hasMotorizedStage e,
                       "motorizedStageName" .= motorizedStageName e,
                       "hasrobot" .= hasRobot e,
                       "robotname" .= robotName e]
