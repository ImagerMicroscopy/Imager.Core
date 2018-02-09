{-# LANGUAGE OverloadedStrings #-}
module EquipmentInitialization
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment
import System.FilePath

import Equipment
import EquipmentMessaging
import EquipmentTypes
import MiscUtils
import RCSerialPort

import Arduino
import Asahi
import Coherent
import DummyEquipment
import Lumencor
import Olympus
import Prior
import Robottor
import Sutter
import Thorlabs

readAvailableEquipment :: IO [EquipmentDescription]
readAvailableEquipment =
    getExecutablePath >>= \exePath ->
    readFile (takeDirectory exePath </> confFilename) >>=
    return . read
    where
        confFilename = "equipment.txt"

withEquipment :: [EquipmentDescription] -> ([EquipmentW] -> IO ()) -> IO ()
withEquipment descs action =
    bracket (initializeEquipment descs) closeEquipment action

initializeEquipment :: [EquipmentDescription] -> IO [EquipmentW]
initializeEquipment descs = mapM initializeDevice descs >>= verifyEquipmentThrows

closeEquipment :: [EquipmentW] -> IO ()
closeEquipment = mapM_ closeDevice

initializeDevice :: EquipmentDescription -> IO EquipmentW
initializeDevice d@(CoherentLightSourceDesc _ _) = initializeCoherent d
initializeDevice d@(LumencorLightSourceDesc _ _) = initializeLumencor d
initializeDevice d@(AsahiLightSourceDesc _ _ _) = initializeAsahiLightSource d
initializeDevice d@(ArduinoLightSourceDesc _ _ _) = initializeArduinoLightSource d
initializeDevice d@(DummyLightSourceDesc _) = initializeDummyLightSource d
initializeDevice d@(ThorlabsFW103HDesc _ _ _) = initializeThorlabsFW130H d
initializeDevice d@(ThorlabsFW102CDesc _ _ _) = initializeThorlabsFW102C d
initializeDevice d@(SutterLambda10BDesc _ _ _) = initializeSutterLambda10B d
initializeDevice d@(OlympusIX71DichroicDesc _ _ _) = initializeOlympusIX71Dichroic d
initializeDevice d@(DummyFilterWheelDesc _ _) = initializeDummyFilterWheel d
initializeDevice d@(PriorDesc _ _) = initializePriorStage d
initializeDevice d@(DummyStageDesc name) = initializeDummyStage d
initializeDevice d@(RobottorDesc name ip port) = initializeRobottor d
initializeDevice _ = error "unknown type of device description"

verifyEquipmentThrows :: [EquipmentW] -> IO [EquipmentW]
verifyEquipmentThrows eqs = when (not (nodups (map equipmentName eqs)))
                                (displayStringThenError "some of the equipment has duplicate names") >>
                            pure eqs
