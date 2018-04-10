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
import GHC.ConsoleHandler
import System.Environment
import System.FilePath

import Equipment
import EquipmentTypes
import MiscUtils
import RCSerialPort

import Arduino
import Asahi
import Coherent
import DummyEquipment
import Lumencor
import Marzhauser
import MicroscopeController
import Olympus
import Oxxius
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
    bracket (initializeEquipment descs) closeEquipment (\eqs ->
        installHandlers eqs >> action eqs)
    where
        installHandlers eqs =
            myThreadId >>= \mainThreadID ->
            installHandler (Catch (f eqs mainThreadID))
        f eqs mainThreadID event =
            case event of
                ControlC -> putStrLn "User Interrupt" >> throwTo mainThreadID UserInterrupt
                Break    -> putStrLn "User Interrupt" >> throwTo mainThreadID UserInterrupt
                Close    -> putStrLn "Close Event"    >> throwTo mainThreadID ThreadKilled >> threadDelay maxBound
                _        -> pure ()

initializeEquipment :: [EquipmentDescription] -> IO [EquipmentW]
initializeEquipment descs = doInit `catch` (\e -> displayStringThenError (displayException (e :: IOException)))
    where
        doInit = forM descs (\desc ->
                     putStr ("Initializing " ++ deviceDescName desc ++ "...") >>
                     initializeDevice desc >>= \dev ->
                     putStr " done!\n" >>
                     pure dev) >>=
                 verifyEquipmentThrows

closeEquipment :: [EquipmentW] -> IO ()
closeEquipment = mapM_ closeDevice

initializeDevice :: EquipmentDescription -> IO EquipmentW
initializeDevice d@(CoherentLightSourceDesc _ _) = initializeCoherent d
initializeDevice d@(LumencorLightSourceDesc _ _) = initializeLumencor d
initializeDevice d@(MarcelLumencorLightSourceDesc _ _ _) = initializeMarcelLumencor d
initializeDevice d@(AsahiLightSourceDesc _ _ _) = initializeAsahiLightSource d
initializeDevice d@(ArduinoLightSourceDesc _ _ _) = initializeArduinoLightSource d
initializeDevice d@(DummyLightSourceDesc _) = initializeDummyLightSource d
initializeDevice d@(MicroscopeControllerDesc _) = initializeMicroscopeController d
initializeDevice d@(ThorlabsFW103HDesc _ _ _) = initializeThorlabsFW130H d
initializeDevice d@(ThorlabsFW102CDesc _ _ _) = initializeThorlabsFW102C d
initializeDevice d@(SutterLambda10BDesc _ _ _) = initializeSutterLambda10B d
initializeDevice d@(OlympusIX71DichroicDesc _ _ _) = initializeOlympusIX71Dichroic d
initializeDevice d@(OxxiusLCDesc _ _) = initializeOxxiusLC d
initializeDevice d@(DummyFilterWheelDesc _ _) = initializeDummyFilterWheel d
initializeDevice d@(PriorDesc _ _) = initializePriorStage d
initializeDevice d@(MarzhauserStageDesc _ _) = initializeMarzhauserStage d
initializeDevice d@(DummyStageDesc _) = initializeDummyStage d
initializeDevice d@(RobottorDesc _ _ _) = initializeRobottor d
initializeDevice _ = error "unknown type of device description"

deviceDescName :: EquipmentDescription -> String
deviceDescName (CoherentLightSourceDesc _ _) = "Coherent laser"
deviceDescName (LumencorLightSourceDesc _ _) = "Lumencor"
deviceDescName (MarcelLumencorLightSourceDesc _ _ _) = "MarcelLumencor"
deviceDescName (AsahiLightSourceDesc _ _ _) = "Asahi lamp"
deviceDescName (ArduinoLightSourceDesc _ _ _) = "Arduino-controlled light source"
deviceDescName (DummyLightSourceDesc _) = "Dummy light source"
deviceDescName (MicroscopeControllerDesc _) = "Microscope"
deviceDescName (ThorlabsFW103HDesc _ _ _) = "Thorlabs FW130H filter wheel"
deviceDescName (ThorlabsFW102CDesc _ _ _) = "ThorlabsFW102C filter wheel"
deviceDescName (SutterLambda10BDesc _ _ _) = "Sutter filter wheel"
deviceDescName (OlympusIX71DichroicDesc _ _ _) = "Olympus IX71 dichroic turret"
deviceDescName (OxxiusLCDesc _ _) = "Oxxius laser combiner"
deviceDescName (DummyFilterWheelDesc _ _) = "Dummy filter wheel"
deviceDescName (PriorDesc _ _) = "Prior motorized stage"
deviceDescName (MarzhauserStageDesc _ _) = "Marzhauser motorized stage"
deviceDescName (DummyStageDesc name) = "Dummy motorized stage"
deviceDescName (RobottorDesc name ip port) = "Robottor"
deviceDescName _ = error "unknown type of device description"

verifyEquipmentThrows :: [EquipmentW] -> IO [EquipmentW]
verifyEquipmentThrows eqs = when (not (nodups (map equipmentName eqs)))
                                (displayStringThenError "some of the equipment has duplicate names") >>
                            pure eqs
