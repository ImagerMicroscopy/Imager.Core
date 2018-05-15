{-# LANGUAGE OverloadedStrings #-}
module DummyEquipment where

import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T

import Equipment
import EquipmentTypes
import FilterUtils

data DummyLightSource = DummyLightSource !EqName
data DummyFilterWheel = DummyFilterWheel !EqName ![(FName, Int)]
data DummyStage = DummyStage !StageName !(IORef StagePosition)

initializeDummyLightSource :: EquipmentDescription -> IO EquipmentW
initializeDummyLightSource (DummyLightSourceDesc name) =
    putStrLn ("opened light source " ++ T.unpack name) >> return (EquipmentW $ DummyLightSource (EqName name))

initializeDummyFilterWheel :: EquipmentDescription -> IO EquipmentW
initializeDummyFilterWheel (DummyFilterWheelDesc name chs) =
    putStrLn ("Opened dummy filter wheel " ++ T.unpack name ++ " with filters " ++ show chs) >>
    return (EquipmentW $ DummyFilterWheel (EqName name) (validateFilters FName (0,5) chs))

initializeDummyStage :: EquipmentDescription -> IO EquipmentW
initializeDummyStage (DummyStageDesc name) =
    putStrLn ("dummy stage " ++ (T.unpack name) ++ " open") >>
    EquipmentW <$> (DummyStage (StageName name) <$> newIORef (StagePosition 0 0 0 True 1))

instance Equipment DummyLightSource where
    equipmentName (DummyLightSource n) = n
    flushSerialPorts _ = pure ()
    closeDevice (DummyLightSource name) = putStr ("closed light source " ++ T.unpack (fromEqName name)) >> return ()
    availableLightSources (DummyLightSource n) =
        [LightSourceDescription (LSName "ls") True True (map LSChannelName ["ch1", "ch2"])]
    activateLightSource (DummyLightSource name) _ chs =
        putStrLn ("activated " ++ T.unpack (fromEqName name) ++ " with channels " ++ show (map fromLSChannelName channels) ++
                  " with powers " ++ show (map fromLSIlluminationPower powers))
        where
            (channels, powers) = unzip chs
    deactivateLightSource (DummyLightSource name) = putStrLn ("deactivated " ++ T.unpack (fromEqName name))

instance Equipment DummyFilterWheel where
    equipmentName (DummyFilterWheel n _) = n
    flushSerialPorts _ = pure ()
    closeDevice (DummyFilterWheel name _) = putStrLn ("Closed filter wheel " ++ T.unpack (fromEqName name))
    availableFilterWheels (DummyFilterWheel n chs) = [FilterWheelDescription (FWName "fw") (map fst chs)]
    switchToFilter (DummyFilterWheel name chs) _ fName =
        putStrLn ("Switched filter wheel " ++ T.unpack (fromEqName name) ++ " to filter " ++ T.unpack (fromFName fName))

instance Equipment DummyStage where
    equipmentName _ = (EqName "Dummy stage")
    flushSerialPorts _ = pure ()
    closeDevice (DummyStage n _) = putStrLn ("dummy stage " ++ (T.unpack (fromStageName n)) ++ " closed")
    hasMotorizedStage _ = True
    motorizedStageName (DummyStage n _) = n
    supportedStageAxes _ = [XAxis, YAxis, ZAxis]
    getStagePosition (DummyStage n posRef) =
        readIORef posRef >>= \pos ->
        putStrLn ("read position of " ++ T.unpack (fromStageName n) ++ " as " ++ show pos) >> pure pos
    setStagePosition (DummyStage n posRef) ds =
        writeIORef posRef ds >>
        putStrLn ("set position of " ++ T.unpack (fromStageName n) ++ " to " ++ show ds)
