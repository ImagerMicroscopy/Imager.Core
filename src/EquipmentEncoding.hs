{-# LANGUAGE OverloadedStrings #-}

module EquipmentEncoding
where

import Data.Aeson

import Equipment
import EquipmentTypes

instance ToJSON EquipmentW where
    toJSON e | hasLightSource e = object ["name" .= lightSourceName e, "channels" .= lightSourceChannels e,
                                     "allowmultiplechannels" .= lightSourceAllowsMultipleChannels e,
                                     "cancontrolpower" .= lightSourceCanControlPower e]
             | hasFilterWheel e = object ["name" .= filterWheelName e, "channels" .= filterWheelChannels e]
             | hasMotorizedStage e = object ["name" .= motorizedStageName e]
             | hasRobot e = object ["name" .= robotName e]
             | otherwise = error "unknown type of object for encoding"
