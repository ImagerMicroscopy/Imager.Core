module EquipmentTypes
where

import Control.Concurrent.MVar
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T

import RCSerialPort

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
                          deriving (Show, Read)
