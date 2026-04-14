{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Equipment.EquipmentTypes
where

import Control.DeepSeq
import Control.Concurrent.MVar
import Data.Aeson
import Data.IORef
import qualified Data.Map as M
import Data.MessagePack
import Data.Text (Text)
import qualified Data.Text as T

newtype EqName = EqName {
                     fromEqName :: Text
                 } deriving (Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON)

newtype LSName = LSName {
                     fromLSName :: Text
                 } deriving (Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON)
newtype LSChannelName = LSChannelName {
                            fromLSChannelName :: Text
                        } deriving (Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON)
newtype LSIlluminationPower = LSIlluminationPower {
                                  fromLSIlluminationPower :: Double
                              } deriving (Show, Eq, Ord, ToJSON, FromJSON)
newtype LSIlluminationDuration = LSIlluminationDuration {
                                     fromLSIlluminationDuration :: Double
                                 } deriving (Show, Eq, Ord, ToJSON, FromJSON)

newtype FWName = FWName {
                     fromFWName :: Text
                 } deriving (Show, Eq, Ord, ToJSON, FromJSON)
newtype FName = FName {
                   fromFName :: Text
                } deriving (Show, Eq, Ord, ToJSON, FromJSON)
newtype StageName = StageName {
                        fromStageName :: Text
                    } deriving (Show, Eq, Ord, ToJSON, FromJSON)

data RobotDescription = RobotDescription {
                            rdName :: !RobotName
                          , rdRobotPrograms :: ![RobotProgram]
                        }
                      deriving (Show)

newtype RobotName = RobotName {
                        fromRobotName :: Text
                    } deriving (Show, Eq, Ord)

newtype RobotProgramName = RobotProgramName {
                               fromRobotProgramName :: Text
                           } deriving (Show, Eq, Ord)

data RobotProgram = RobotProgram {
                        rpName :: !RobotProgramName
                      , rpArguments :: ![RobotProgramArgumentDescription]
                    } deriving (Show)

data RobotProgramArgumentDescription = DiscreteRobotProgramArgumentDescription {
                                           dradArgumentName :: !Text
                                         , dradPermissibleArgumentValues :: ![Text]
                                       }
                                     | ContinuousRobotProgramArgumentDescription {
                                           cradArgumentName :: !Text
                                         , cradMinValue :: !Double
                                         , cradMaxValue :: !Double
                                         , cradIncrement :: !Double
                                       }
                                    deriving (Show)

data RobotProgramArgument = DiscreteRobotProgramArgument {
                                drpaArgumentName :: !Text
                              , drpaValue :: !Text}
                          | ContinuousRobotProgramArgument {
                              crpaArgumentName :: !Text
                            , crpaValue :: !Double}
                          deriving (Show)

data RobotProgramCallParams = RobotProgramCallParams {
                                  rpcpProgramName :: !RobotProgramName
                                , rpcpArguments :: ![RobotProgramArgument]
                              } deriving (Show)

data StagePosition = StagePosition {
                         spX :: !Double
                       , spY :: !Double
                       , spZ :: !Double
                       , spUsingHardwareAutoFocus :: !Bool
                       , spHardwareAFOffset :: !Int
                     } deriving (Show)

instance NFData StagePosition where
    rnf (StagePosition x y z u o) = x `seq` y `seq` z `seq` u `seq` o `seq` ()

instance ToJSON StagePosition where
    toJSON p = object ["x" .= spX p, "y" .= spY p, "z" .= spZ p,
                       "usinghardwareautofocus" .= spUsingHardwareAutoFocus p,
                       "hardwareautofocusoffset" .= spHardwareAFOffset p]

instance FromJSON StagePosition where
    parseJSON (Object v) = StagePosition <$> v .: "x" <*> v .: "y" <*> v .: "z"
                                         <*> v .: "usinghardwareautofocus"
                                         <*> v .: "hardwareautofocusoffset"
    parseJSON _ = fail "expected a JSON object"
  
instance MessagePack StagePosition where
    toObject p = toObject $ M.fromList ([
                    ("x" :: Text, toObject $ spX p),
                    ("y", toObject $ spY p),
                    ("z", toObject $ spZ p),
                    ("usinghardwareautofocus", toObject $ spUsingHardwareAutoFocus p),
                    ("hardwareautofocusoffset", toObject $ spHardwareAFOffset p)
                  ])

data LightSourceDescription = LightSourceDescription {
                                  lsdName :: !LSName
                                , lsdCanControlPower :: !Bool
                                , lsdAllowsMultipleChannels :: !Bool
                                , lsdChannels :: ![LSChannelName]
                              }

data MovableComponentDescription = DiscreteMovableComponent {
                                       componentName :: !Text
                                     , possibleSettings :: ![Text]
                                   }
                                 | ContinuouslyMovableComponent {
                                       componentName :: !Text
                                     , minValue :: !Double
                                     , maxValue :: !Double
                                     , increment :: !Double -- allowable increment between minValue and maxValue. <=0 means no discrete increment
                                   }
                                   deriving (Eq, Show)

instance ToJSON MovableComponentDescription where
    toJSON c@(DiscreteMovableComponent{}) =
        object ["componentname" .= c.componentName, "possiblesettings" .= c.possibleSettings,
                "type" .= ("discretemovablecomponent" :: Text)]
    toJSON c@(ContinuouslyMovableComponent{}) =
        object ["componentname" .= c.componentName, "minvalue" .= c.minValue,
                "maxvalue" .= c.maxValue, "increment" .= c.increment, "type" .= ("continuousmovablecomponent" :: Text)]

instance FromJSON MovableComponentDescription where
    parseJSON (Object v) =
        v .: "type" >>= \(t :: Text) ->
        case (t) of
            "discretemovablecomponent" ->
                DiscreteMovableComponent <$> v .: "componentname"
                                         <*> v .: "possiblesettings"
            "continuousmovablecomponent" ->
                ContinuouslyMovableComponent <$> v .: "componentname"
                                             <*> v .: "minvalue"
                                             <*> v .: "maxvalue"
                                             <*> v .: "increment"
            _ -> fail "not a MovableComponentDescription type"
    parseJSON _ = fail "not a MovableComponentDescription"

data MovableComponentSetting = DiscreteComponentSetting {
                                   mcsComponentName :: !Text
                                 , mcsDesiredStrSetting :: !Text
                               }
                             | ContinuousComponentSetting {
                                   mcsComponentName :: !Text
                                 , mcsDesiredNumSetting :: !Double
                               }
                              deriving (Eq, Show)

instance ToJSON MovableComponentSetting where
    toJSON c@(DiscreteComponentSetting{}) =
        object ["componentname" .= c.mcsComponentName, "desiredsetting" .= c.mcsDesiredStrSetting,
                "type" .= ("discretemovablesetting" :: Text)]
    toJSON c@(ContinuousComponentSetting{}) =
        object ["componentname" .= c.mcsComponentName, "desiredsetting" .= c.mcsDesiredNumSetting,
                "type" .= ("continuousmovablesetting" :: Text)]

instance FromJSON MovableComponentSetting where
    parseJSON (Object v) =
        v .: "type" >>= \(t :: Text) ->
        case (t) of
            "discretemovablesetting" ->
                DiscreteComponentSetting <$> v .: "componentname"
                                         <*> v .: "desiredsetting"
            "continuousmovablesetting" ->
                ContinuousComponentSetting <$> v .: "componentname"
                                          <*> v .: "desiredsetting"
            _ -> fail "not a MovableComponentSetting type"
    parseJSON _ = fail "not a MovableComponentSetting"

data StageAxis = XAxis | YAxis | ZAxis
               deriving (Eq, Show)


data WantDigitalModulation = DigitalModulation 
                           | NoModulation
                           | AnalogModulation
                           deriving (Eq, Show, Read)

data EquipmentDescription = CoherentLightSourceDesc {
                                clsdName :: !Text
                              , clsdSerialPortName :: !String
                            }
                          | LumencorLightSourceDesc {
                                llsdName :: !Text
                              , llsdSerialPortName :: !String
                            }
                          | MarcelLumencorLightSourceDesc {
                                mllsdName :: !Text
                              , mllsdLumencorPortName :: !String
                              , mllsdArduinoPortName :: !String
                            }
                          | AsahiLightSourceDesc {
                                alsName :: !Text
                              , alsSerialPortName :: !String
                              , alsFilters :: ![(Text, Int)]
                            }
                          | ASITigerControllerDesc {
                                asisName :: !Text
                              , asisFilter :: ![(Text, Int)]
                              , asisPort :: !String
                            }
                          | ArduinoLightSourceDesc {
                                ardName :: !Text
                              , ardSerialPortName :: !String
                              , ardChannels :: ![(Text, (Int, Double))]
                            }
                          | PWMLaserControllerDesc {
                                pwrName :: !Text
                              , pwrSerialPortName :: !String
                            }
                          | MultiModeLasersDesc {
                                mmlName :: !Text
                              , mmlSerialPortName :: !String
                            }
                          | BlueBoxNijiDesc {
                                bbnDescName :: !Text
                              , bbnDescPortName :: !String
                            }
                          | DummyLightSourceDesc {
                                dlsdName :: !Text
                            }
                          | DummyRobotDesc {
                                drdName :: !Text
                            }
                          | ThorlabsFW103HDesc {
                                fw103DescName :: !Text
                              , fw103DescPortName :: !String
                              , fw103DescFilters :: [(Text, Int)]
                            }
                          | ThorlabsFW102CDesc {
                                fw102CDescName :: !Text
                              , fw102CDescPortName :: !String
                              , fw102CDescFilters :: [(Text, Int)]
                            }
                          | OlympusIX71DichroicDesc {
                                ix71DescName :: !Text
                              , ix71DescPortName :: !String
                              , ix71DescFilters :: ![(Text, Int)]
                            }
                          | OxxiusLBXDesc {
                                oxxLBXDescName :: !Text
                              , oxxLBXDescPortName :: !String
                            }
                          | OxxiusLCDesc {
                                oxxDescName :: !Text
                              , oxxDescPortName :: !String
                              , oxxModulationMode :: !WantDigitalModulation
                          }
                          | MarcelOxxiusLCDesc {
                                moxxDescName :: !Text
                              , moxxDescPortName :: !String
                              , moxxDescArduinoPortName :: !String
                          }
                          | SutterLambda10BDesc {
                                sl10BDescName :: !Text
                              , sl10BDescPortName :: !String
                              , sl10BDescFilters :: [(Text, Int)]
                            }
                          | DummyFilterWheelDesc {
                                dfwDescName :: !Text
                              , dfwDescFilters :: [(Text, Int)]
                            }
                          | PriorDesc {
                                psDescName :: !Text
                              , psDescPortName :: !String
                            }
                          | PIStageDesc {
                                pisDescName :: !Text
                              , pisDescPortName :: !String
                          }
                          | MarzhauserStageDesc {
                                msDescName :: !Text
                              , msDescPortName :: !String
                            }
                          | DummyStageDesc {
                                dsName :: !Text
                            }
                          | RobottorDesc {
                              roDescName :: !Text
                            , roDescIPAddress :: !String
                            , roDescPortNum :: !Int
                            }
                         | RemoteStageDesc {
                              rsDescName :: !Text
                            , rsDescIPAddress :: !String
                            , rsDescPortNum :: !Int
                          }
                          deriving (Show, Read)
