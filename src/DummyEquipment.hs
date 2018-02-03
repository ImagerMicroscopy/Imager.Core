{-# LANGUAGE OverloadedStrings #-}
module DummyEquipment where

import Data.Text (Text)
import qualified Data.Text as T

import Equipment
import EquipmentTypes
import FilterUtils

data DummyLightSource = DummyLightSource !Text
data DummyFilterWheel = DummyFilterWheel !Text ![(Text, Int)]
data DummyStage = DummyStage !Text

initializeDummyLightSource :: EquipmentDescription -> IO EquipmentW
initializeDummyLightSource (DummyLightSourceDesc name) =
    putStrLn ("opened light source " ++ T.unpack name) >> return (EquipmentW $ DummyLightSource name)

initializeDummyFilterWheel :: EquipmentDescription -> IO EquipmentW
initializeDummyFilterWheel (DummyFilterWheelDesc name chs) =
    putStrLn ("Opened dummy filter wheel " ++ T.unpack name ++ " with filters " ++ show chs) >>
    return (EquipmentW $ DummyFilterWheel name (validateFilters (0,5) chs))

initializeDummyStage :: EquipmentDescription -> IO EquipmentW
initializeDummyStage (DummyStageDesc name) =
    putStrLn ("dummy stage " ++ (T.unpack name) ++ " open") >>
    return (EquipmentW $ DummyStage name)

instance Equipment DummyLightSource where
    equipmentName _ = "Dummy light source"
    closeDevice (DummyLightSource name) = putStr ("closed light source " ++ T.unpack name) >> return ()
    hasLightSource _ = True
    lightSourceName (DummyLightSource n) = n
    lightSourceCanControlPower _ = True
    lightSourceAllowsMultipleChannels _ = True
    lightSourceChannels _ = ["ch1", "ch2"]
    activateLightSource (DummyLightSource name) chs =
        putStrLn ("activated " ++ T.unpack name ++ " with channels " ++ show channels ++ " with powers " ++ show powers)
        where
            (channels, powers) = unzip chs
    deactivateLightSource (DummyLightSource name) = putStrLn ("deactivated " ++ T.unpack name)

instance Equipment DummyFilterWheel where
    equipmentName _ = "Dummy filter wheel"
    closeDevice (DummyFilterWheel name _) = putStrLn ("Closed filter wheel " ++ T.unpack name)
    hasFilterWheel _ = True
    filterWheelName (DummyFilterWheel n _) = n
    filterWheelChannels (DummyFilterWheel _ chs) = map fst chs
    switchToFilter (DummyFilterWheel name chs) chName =
        putStrLn ("Switched filter wheel " ++ T.unpack name ++ " to filter " ++ T.unpack chName)

instance Equipment DummyStage where
    equipmentName _ = "Dummy stage"
    closeDevice (DummyStage n) = putStrLn ("dummy stage " ++ (T.unpack n) ++ " closed")
    hasMotorizedStage _ = True
    motorizedStageName (DummyStage n) = n
    getStagePosition (DummyStage n) =
        putStrLn ("read position of " ++ T.unpack n) >> return (0.0, 0.0, 0.0)
    setStagePosition (DummyStage n) ds =
        putStrLn ("set position of " ++ T.unpack n ++ " to " ++ show ds)
