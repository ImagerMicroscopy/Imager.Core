{-# LANGUAGE OverloadedStrings #-}
module Equipment.EquipmentInitialization
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
import System.Signal

import AggregateMotorizedStage
import Equipment.Equipment
import Equipment.EquipmentPlugins
import Equipment.EquipmentTypes
import Utils.MiscUtils
import RCSerialPort

import Equipment.Devices.Arduino
import Equipment.Devices.ASITigerController
import Equipment.Devices.PWMLaserController
import Equipment.Devices.Asahi
import Equipment.Devices.BlueBoxOptics
import Equipment.Devices.Coherent
import Dummy.DummyEquipment
import Equipment.Devices.Lumencor
import Equipment.Devices.Marzhauser
import Equipment.Devices.Olympus
import Equipment.Devices.Oxxius
import Equipment.Devices.MarcelOxxius
import Equipment.Devices.Prior
import Equipment.Devices.PIStage
import Equipment.Devices.Robottor
import Equipment.Devices.Sutter
import Equipment.Devices.Thorlabs
import Equipment.Devices.RemoteStage
import Equipment.Devices.MultiModeLasers
import Equipment.Devices.OxxiusLBX

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
        bracket (loadPlugins) closeEquipment (\pluginEqs ->
            let allEquipment = aggregateMotorizedStagesIfNeeded (eqs ++ pluginEqs)
            in  verifyEquipmentThrows allEquipment >>
                installHandlers >> action allEquipment))
    where
        installHandlers =
            myThreadId >>= \mainThreadID ->
            installHandler sigINT (f mainThreadID "User Interrupt") >>
            installHandler sigTERM (f mainThreadID "Termination Request")
        f mainThreadID desc _ = putStrLn desc >> throwTo mainThreadID UserInterrupt

initializeEquipment :: [EquipmentDescription] -> IO [EquipmentW]
initializeEquipment descs = doInit `catch` (\e -> displayStringThenError (displayException (e :: IOException)))
    where
        doInit = forM descs (\desc ->
                     putStr ("Initializing " ++ deviceDescName desc ++ "...") >>
                     initializeDevice desc >>= \dev ->
                     putStr " done!\n" >>
                     pure dev)

closeEquipment :: [EquipmentW] -> IO ()
closeEquipment = mapM_ closeDevice

initializeDevice :: EquipmentDescription -> IO EquipmentW
initializeDevice d@(CoherentLightSourceDesc {}) = initializeCoherent d
initializeDevice d@(LumencorLightSourceDesc {}) = initializeLumencor d
initializeDevice d@(MarcelLumencorLightSourceDesc {}) = initializeMarcelLumencor d
initializeDevice d@(AsahiLightSourceDesc {}) = initializeAsahiLightSource d
initializeDevice d@(ASITigerControllerDesc {}) = initializeASITigerController d
initializeDevice d@(ArduinoLightSourceDesc {}) = initializeArduinoLightSource d
initializeDevice d@(PWMLaserControllerDesc {}) = initializePWMLaserControllerLightSource d
initializeDevice d@(MultiModeLasersDesc {}) = initializeMultiModeLasersLightSource d
initializeDevice d@(BlueBoxNijiDesc {}) = initializeBlueBoxNiji d
initializeDevice d@(DummyLightSourceDesc {}) = initializeDummyLightSource d
initializeDevice d@(ThorlabsFW103HDesc {}) = initializeThorlabsFW130H d
initializeDevice d@(ThorlabsFW102CDesc {}) = initializeThorlabsFW102C d
initializeDevice d@(SutterLambda10BDesc {}) = initializeSutterLambda10B d
initializeDevice d@(OlympusIX71DichroicDesc {}) = initializeOlympusIX71Dichroic d
initializeDevice d@(OxxiusLBXDesc {}) = initializeOxxiusLBX d
initializeDevice d@(OxxiusLCDesc {}) = initializeOxxiusLC d
initializeDevice d@(MarcelOxxiusLCDesc {}) = initializeMarcelOxxiusLC d
initializeDevice d@(DummyFilterWheelDesc {}) = initializeDummyFilterWheel d
initializeDevice d@(PriorDesc {}) = initializePriorStage d
initializeDevice d@(PIStageDesc {}) = initializePIStage d
initializeDevice d@(MarzhauserStageDesc {}) = initializeMarzhauserStage d
initializeDevice d@(DummyStageDesc {}) = initializeDummyStage d
initializeDevice d@(RobottorDesc {}) = initializeRobottor d
initializeDevice d@(RemoteStageDesc {}) = initializeRemoteStage d
initializeDevice d = error ("unknown type of device description: " ++ show d)

deviceDescName :: EquipmentDescription -> String
deviceDescName (CoherentLightSourceDesc {}) = "Coherent laser"
deviceDescName (LumencorLightSourceDesc {}) = "Lumencor"
deviceDescName (MarcelLumencorLightSourceDesc {}) = "MarcelLumencor"
deviceDescName (AsahiLightSourceDesc {}) = "Asahi lamp"
deviceDescName (ASITigerControllerDesc {}) = "ASI Tiger controller"
deviceDescName (ArduinoLightSourceDesc {}) = "Arduino-controlled light source"
deviceDescName (PWMLaserControllerDesc {}) = "PWM Laser Controller"
deviceDescName (MultiModeLasersDesc {}) = "Multimode lasers controller"
deviceDescName (BlueBoxNijiDesc {}) = "BlueBox Niji"
deviceDescName (DummyLightSourceDesc {}) = "Dummy light source"
deviceDescName (ThorlabsFW103HDesc {}) = "Thorlabs FW130H filter wheel"
deviceDescName (ThorlabsFW102CDesc {}) = "ThorlabsFW102C filter wheel"
deviceDescName (SutterLambda10BDesc {}) = "Sutter filter wheel"
deviceDescName (OlympusIX71DichroicDesc {}) = "Olympus IX71 dichroic turret"
deviceDescName (OxxiusLBXDesc {}) = "Oxxius LBX laser module"
deviceDescName (OxxiusLCDesc {}) = "Oxxius laser combiner"
deviceDescName (MarcelOxxiusLCDesc {}) = "MarcelOxxius"
deviceDescName (DummyFilterWheelDesc {}) = "Dummy filter wheel"
deviceDescName (PriorDesc {}) = "Prior motorized stage"
deviceDescName (PIStageDesc {}) = "PI Stage"
deviceDescName (MarzhauserStageDesc {}) = "Marzhauser motorized stage"
deviceDescName (DummyStageDesc {}) = "Dummy motorized stage"
deviceDescName (RobottorDesc {}) = "Robottor"
deviceDescName (RemoteStageDesc {}) = "RemoteStage"
deviceDescName d = error ("unknown type of device description " ++ show d)

verifyEquipmentThrows :: [EquipmentW] -> IO [EquipmentW]
verifyEquipmentThrows eqs = when (not (nodups (map equipmentName eqs)))
                                (displayStringThenError "some of the equipment has duplicate names") >>
                            pure eqs
