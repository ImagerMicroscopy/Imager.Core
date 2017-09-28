module EquipmentTypes
where

import Control.Concurrent.MVar
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T

import RCSerialPort

data Equipment = -- light sources
                 CoherentLightSource !Text !SerialPort !(IORef (Bool, Double, Double)) !(IORef (Bool, Double))
               | LumencorLightSource !Text !SerialPort !(IORef Bool) !(IORef LumencorFilter)
               | AsahiLightSource !Text ![(Text, (Int, Double))] !SerialPort
               | ArduinoLightSource !Text ![(Text, (Int, Double))] !(IORef [(Int, Double)]) !SerialPort
               | DummyLightSource !Text
               -- filter wheels
               | ThorlabsFW103H !Text ![(Text, Int)] !SerialPort
               | ThorlabsFW102C !Text ![(Text, Int)] !SerialPort
               | SutterLambda10B !Text ![(Text, Int)] !SerialPort
               | OlympusIX71Dichroic !Text ![(Text, Int)] !(IORef (Bool, Int)) !SerialPort
               | DummyFilterWheel !Text ![(Text, Int)]
               -- motorized stage
               | PriorStage {
                     psName :: !Text
                   , psPort :: !(MVar SerialPort)
                 }
               | DummyStage !Text
               -- robots
               | Robottor {
                     roName :: !Text
                   , roIPAddress :: !String
                   , roPortNum :: !Int
                 }

data EquipmentDescription = CoherentLightSourceDesc {
                                clsdName :: !Text
                              , clsdSerialPortName :: !String
                            }
                          | LumencorLightSourceDesc {
                                llsdName :: !Text
                              , llsdSerialPortName :: !String
                            }
                          | AsahiLightSourceDesc {
                                alsName :: !Text
                              , alsSerialPortName :: !String
                              , alsFilters :: ![(Text, (Int, Double))]
                            }
                          | ArduinoLightSourceDesc {
                                ardName :: !Text
                              , ardSerialPortName :: !String
                              , ardChannels :: ![(Text, (Int, Double))]
                            }
                          | DummyLightSourceDesc {
                                dlsdName :: !Text
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
                          | DummyStageDesc {
                                dsName :: !Text
                            }
                          | RobottorDesc {
                              roDescName :: !Text
                            , roDescIPAddress :: !String
                            , roDescPortNum :: !Int
                          }
                          deriving (Show, Read)
                          
data LumencorChannel = LCViolet | LCBlue | LCCyan |LCTeal
                     | LCGreen | LCYellow | LCRed
                       deriving (Eq)
data LumencorFilter = LCGreenFilter | LCYellowFilter
                      deriving (Eq)

data ArduinoPinState = ArduinoInput | ArduinoOutput

isLightSource :: Equipment -> Bool
isLightSource (CoherentLightSource _ _ _ _) = True
isLightSource (LumencorLightSource _ _ _ _) = True
isLightSource (AsahiLightSource _ _ _) = True
isLightSource (ArduinoLightSource _ _ _ _) = True
isLightSource (DummyLightSource _) = True
isLightSource _ = False
               
isFilterWheel :: Equipment -> Bool
isFilterWheel (ThorlabsFW103H _ _ _) = True
isFilterWheel (ThorlabsFW102C _ _ _) = True
isFilterWheel (SutterLambda10B _ _ _) = True
isFilterWheel (OlympusIX71Dichroic _ _ _ _) = True
isFilterWheel (DummyFilterWheel _ _) = True
isFilterWheel _ = False

isMotorizedStage :: Equipment -> Bool
isMotorizedStage (PriorStage _ _) = True
isMotorizedStage (DummyStage _) = True
isMotorizedStage _ = False

isRobot :: Equipment -> Bool
isRobot (Robottor _ _ _) = True
isRobot _ = False
