{-# LANGUAGE OverloadedStrings #-}

module Encodings.EquipmentEncoding
where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Equipment.Equipment
import Equipment.EquipmentTypes

instance ToJSON LightSourceDescription where
    toJSON l = object ["name" .= lsdName l, "cancontrolpower" .= lsdCanControlPower l,
                       "allowmultiplechannels" .= lsdAllowsMultipleChannels l,
                       "channels" .= lsdChannels l]

instance ToJSON EquipmentW where
    toJSON e = object ["name" .= equipmentName e,
                       "availablelightsources" .= availableLightSources e,
                       "availablemovablecomponents" .= availableMovableComponents e,
                       "hasmotorizedstage" .= hasMotorizedStage e,
                       "motorizedstageName" .= stageN,
                       "availablerobots" .= availableRobots e]
       where
           stageN = if (hasMotorizedStage e) then (fromStageName (motorizedStageName e)) else T.empty

instance ToJSON RobotName where
    toJSON (RobotName n) = toJSON n
instance FromJSON RobotName where
    parseJSON = fmap RobotName . parseJSON
instance ToJSON RobotProgramName where
    toJSON (RobotProgramName n) = toJSON n
instance FromJSON RobotProgramName where
    parseJSON = fmap RobotProgramName . parseJSON

instance ToJSON RobotDescription where
    toJSON d = object ["robotname" .= rdName d, "robotprograms" .= rdRobotPrograms d]

instance ToJSON RobotProgram where
    toJSON p = object ["programname" .= rpName p, "programarguments" .= rpArguments p]

instance ToJSON RobotProgramArgumentDescription where
    toJSON a@DiscreteRobotProgramArgumentDescription{} = object ["type" .= ("discreterobotprogramargument" :: Text),
                                                                 "programargumentname" .= dradArgumentName a,
                                                                 "permissiblevalues" .= dradPermissibleArgumentValues a]
    toJSON a@ContinuousRobotProgramArgumentDescription{} = object ["type" .= ("continuousrobotprogramargument" :: Text),
                                                                  "programargumentname" .= cradArgumentName a,
                                                                  "minvalue" .= cradMinValue a,
                                                                  "maxvalue" .= cradMaxValue a,
                                                                  "increment" .= cradIncrement a]

instance FromJSON RobotProgramArgumentDescription where
    parseJSON (Object v) = do
        argType :: Text <- v .: "type"
        case argType of
            "discreterobotprogramargument" -> do
                argumentName <- v .: "programargumentname"
                permissibleValues <- v .: "permissiblevalues"
                return $ DiscreteRobotProgramArgumentDescription argumentName permissibleValues
            "continuousrobotprogramargument" -> do
                argumentName <- v .: "programargumentname"
                minValue <- v .: "minvalue"
                maxValue <- v .: "maxvalue"
                increment <- v .: "increment"
                return $ ContinuousRobotProgramArgumentDescription argumentName minValue maxValue increment
            _ -> fail "Unknown argument type"

instance ToJSON RobotProgramArgument where
    toJSON (DiscreteRobotProgramArgument arg) =
        object ["robotprogramargumenttype" .= ("discrete" :: Text),
                "argument" .= arg]
    toJSON (ContinuousRobotProgramArgument arg) =
        object ["robotprogramargumenttype" .= ("continuous" :: Text),
                "argument" .= arg]

instance FromJSON RobotProgramArgument where
    parseJSON (Object v) = 
        v .: "robotprogramargumenttype" >>= \(argType :: Text) ->
        case argType of
            "discrete"  -> DiscreteRobotProgramArgument <$> v .: "argument"
            "continuous" -> ContinuousRobotProgramArgument <$> v .: "argument"
