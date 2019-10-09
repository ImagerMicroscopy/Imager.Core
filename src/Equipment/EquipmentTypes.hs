{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Equipment.EquipmentTypes
where

import Control.DeepSeq
import Control.Concurrent.MVar
import Data.Aeson
import Data.IORef
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
newtype RobotName = RobotName {
                        fromRobotName :: Text
                    } deriving (Show, Eq, Ord, ToJSON, FromJSON)
newtype RobotProgramName = RobotProgramName {
                               fromRobotProgramName :: Text
                           } deriving (Show, Eq, Ord, ToJSON, FromJSON)

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

data LightSourceDescription = LightSourceDescription {
                                  lsdName :: !LSName
                                , lsdCanControlPower :: !Bool
                                , lsdAllowsMultipleChannels :: !Bool
                                , lsdChannels :: ![LSChannelName]
                              }
data FilterWheelDescription = FilterWheelDescription {
                                  fwdName :: !FWName
                                , fwdFilters :: ![FName]
                              }

data StageAxis = XAxis | YAxis | ZAxis
               deriving (Eq, Show)

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
                          | ArduinoLightSourceDesc {
                                ardName :: !Text
                              , ardSerialPortName :: !String
                              , ardChannels :: ![(Text, (Int, Double))]
                            }
                          | BlueBoxNijiDesc {
                                bbnDescName :: !Text
                              , bbnDescPortName :: !String
                            }
                          | DummyLightSourceDesc {
                                dlsdName :: !Text
                            }
                          | MicroscopeControllerDesc {
                                mcdName :: !Text
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
                          | OxxiusLCDesc {
                                oxxDescName :: !Text
                              , oxxDescPortName :: !String
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
