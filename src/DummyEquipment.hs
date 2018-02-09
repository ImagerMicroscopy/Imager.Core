{-# LANGUAGE OverloadedStrings #-}
module DummyEquipment where

import Data.Text (Text)
import qualified Data.Text as T

import Equipment
import EquipmentTypes
import FilterUtils

data DummyLightSource = DummyLightSource !EqName
data DummyFilterWheel = DummyFilterWheel !EqName ![(FName, Int)]
data DummyStage = DummyStage !StageName

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
    return (EquipmentW $ DummyStage (StageName name))

instance Equipment DummyLightSource where
    equipmentName (DummyLightSource n) = n
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
    closeDevice (DummyFilterWheel name _) = putStrLn ("Closed filter wheel " ++ T.unpack (fromEqName name))
    availableFilterWheels (DummyFilterWheel n chs) = [FilterWheelDescription (FWName "fw") (map fst chs)]
    switchToFilter (DummyFilterWheel name chs) _ fName =
        putStrLn ("Switched filter wheel " ++ T.unpack (fromEqName name) ++ " to filter " ++ T.unpack (fromFName fName))

instance Equipment DummyStage where
    equipmentName _ = (EqName "Dummy stage")
    closeDevice (DummyStage n) = putStrLn ("dummy stage " ++ (T.unpack (fromStageName n)) ++ " closed")
    hasMotorizedStage _ = True
    motorizedStageName (DummyStage n) = n
    getStagePosition (DummyStage n) =
        putStrLn ("read position of " ++ T.unpack (fromStageName n)) >> return (0.0, 0.0, 0.0)
    setStagePosition (DummyStage n) ds =
        putStrLn ("set position of " ++ T.unpack (fromStageName n) ++ " to " ++ show ds)
