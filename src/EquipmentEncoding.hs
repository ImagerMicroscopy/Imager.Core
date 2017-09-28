{-# LANGUAGE OverloadedStrings #-}

module EquipmentEncoding
where

import Data.Aeson

import EquipmentTypes
import FilterWheel
import LightSources
import MotorizedStage
import Robot

instance ToJSON Equipment where
    toJSON e | isLightSource e = object ["name" .= lightSourceName e, "channels" .= lightSourceChannels e,
                                     "allowmultiplechannels" .= lightSourceAllowsMultipleChannels e,
                                     "cancontrolpower" .= lightSourceCanControlPower e]
             | isFilterWheel e = object ["name" .= filterWheelName e, "channels" .= filterWheelChannels e]
             | isMotorizedStage e = object ["name" .= motorizedStageName e]
             | isRobot e = object ["name" .= robotName e]
             | otherwise = error "unknown type of object for encoding"
