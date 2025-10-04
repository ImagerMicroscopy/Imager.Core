{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Measurements.MeasurementProgramVerification (
    validateMeasurementElementThrows
  , validateMeasurementElement
  , foldMeasurementElement
) where

import Control.Exception
import Data.Either
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Monad
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Detectors.Detector
import Equipment.Equipment
import Equipment.EquipmentTypes
import Measurements.MeasurementProgramTypes
import Utils.MeasurementProgramUtils
import Utils.MiscUtils

type RobotInfo = (Text, [Text]) -- robot name, robot programs

validateMeasurementElementThrows :: Detector a => [a] -> [EquipmentW] -> MeasurementElement -> DefinedDetections -> IO ()
validateMeasurementElementThrows dets eqs me ddets =
    pure ((validateDefinedDetections dets ddets) ++ (verifyMeasurementElementIDsAreUnique me) ++ (verifyRobotElements eqs me)) >>= \result ->
    when (not $ null result) (
        throwIO (userError $ head result)) >>
    case (foldMeasurementElement (validateMeasurementElement eqs ddets) me) of
                                                          (e : xs) -> throwIO (userError e)
                                                          []       -> return ()

validateDefinedDetections :: Detector a => [a] -> DefinedDetections -> [String]
validateDefinedDetections dets ddets =
    let availableDetectorNames = map detectorName dets
        reqDetectorNames = mconcat (map (map dtpDetectorName . dpDetectors) (M.elems ddets))
    in  if (all (`elem` availableDetectorNames) reqDetectorNames)
        then []
        else ["unknown detector"]

-- should not inspect contained MeasurementElements
-- empty list for no error
validateMeasurementElement :: [EquipmentW] -> DefinedDetections -> MeasurementElement -> [String]
validateMeasurementElement eqs ddets (MEDetection _ detNames programIDs)
    | null detNames = ["no detection specified"]
    | any (\dn -> not (M.member dn ddets)) detNames = ["one or more detection names are undefined"]
    | otherwise = concat $ (map (validateDetection eqs) dets)
    where
        dets = map (\dn -> fromJust $ M.lookup dn ddets) detNames
validateMeasurementElement eqs _ (MEIrradiation _ dur ips)
    | (fromLSIlluminationDuration dur < 0.0) || (fromLSIlluminationDuration dur > 60) = ["invalid irradiation duration: " ++ show dur]
    | otherwise = concat $ (map (validateIrradiation eqs) ips)
validateMeasurementElement eqs _ (MEWait _ (WaitDuration dur))
    | (dur < 0.0) || (dur > 3600) = ["invalid wait duration: " ++ show dur]
    | otherwise = []
validateMeasurementElement eqs _ (MEExecuteRobotProgram _ _) = []
validateMeasurementElement eqs _ (MEDoTimes _ (NumIterationsTotal n) _ es)
    | (n < 0) || (n > (floor 10e6)) = ["invalid number of times to repeat: " ++ show n]
    | null es = ["do times loop but no actions"]
    | otherwise = []
validateMeasurementElement eqs _ (MEFastAcquisitionLoop _ (NumIterationsTotal n) (detName, detParams) inputProgramID outputProgramIDs)
    | (n < 0) || (n > (floor 10e6)) = ["invalid number of times to repeat: " ++ show n]
    | otherwise = validateDetection eqs detParams
validateMeasurementElement eqs _ (METimeLapse _ (NumIterationsTotal n) (WaitDuration dur) _ es)
    | (n < 0) || (n > (floor 10e6)) = ["invalid number of times to repeat: " ++ show n]
    | (dur < 0.0) || (dur > 3600 * 2) = ["invalid time lapse duration: " ++ show dur]
    | null es = ["timelapse loop but no elements"]
    | otherwise = []
validateMeasurementElement eqs _ (MEStageLoop _ stageName pos _ es)
    | null pos = ["no positions in stage loop"]
    | T.null (fromStageName stageName) = ["no stage name"]
    | stageName `notElem` stageNames = ["can't find stage named " ++ T.unpack (fromStageName stageName)]
    | null es = ["stage loop but no elements"]
    | otherwise = []
    where
        stageNames = map motorizedStageName (filter hasMotorizedStage eqs)
validateMeasurementElement eqs _ (MERelativeStageLoop _ stageName (RelativeStageLoopParams dx dy dz (bx, ax) (by, ay) (bz, az) _) _ es)
    | T.null (fromStageName stageName) = ["no stage name"]
    | stageName `notElem` stageNames = ["can't find stage named " ++ T.unpack (fromStageName stageName)]
    | null es = ["relative stage loop but no elements"]
    | (dx < 0) || (dy < 0) || (dz < 0) = ["invalid additional planes distance"]
    | (bx < 0) || (ax < 0) = ["invalid additional planes x"]
    | (by < 0) || (ay < 0) = ["invalid additional planes y"]
    | (bz < 0) || (az < 0) = ["invalid additional planes z"]
    | otherwise = []
    where
        stageNames = map motorizedStageName (filter hasMotorizedStage eqs)

verifyMeasurementElementIDsAreUnique :: MeasurementElement -> [String] -- empty list means no error
verifyMeasurementElementIDsAreUnique me =
    let allElementIDs = foldMeasurementElement f me
        nIDs = length allElementIDs
        nUniqueIDs = length (nub allElementIDs)
    in  if (nIDs == nUniqueIDs)
        then []
        else ["measurement element IDs are not unique"]
    where
        f :: MeasurementElement -> [ElementID]
        f e = [getElementID e]

getElementID :: MeasurementElement -> ElementID
getElementID (MEDetection elementID _ ids) = elementID
getElementID (MEIrradiation elementID _ _) = elementID
getElementID (MEWait elementID _) = elementID
getElementID (MEFastAcquisitionLoop elementID _ _ _ _) = elementID
getElementID (MEExecuteRobotProgram elementID _) = elementID
getElementID (MEDoTimes elementID _ _ _) = elementID
getElementID (METimeLapse elementID _ _ _ _) = elementID
getElementID (MEStageLoop elementID _ _ _ _) = elementID
getElementID (MERelativeStageLoop elementID _ _ _ _) = elementID

verifyRobotElements :: [EquipmentW] -> MeasurementElement -> [String]
verifyRobotElements eqs me = filter (not . null) . map (verifyRobotElement eqs) $ allRobotMeasurementElements
    where
        allRobotMeasurementElements = foldMeasurementElement f me
            where
                f m@(MEExecuteRobotProgram _ RobotProgramExecutionParams{}) = [m]
                f _ = []
        allRobotDescriptions = concat (map availableRobots eqs)
        verifyRobotElement :: [EquipmentW] -> MeasurementElement -> String
        verifyRobotElement eqs (MEExecuteRobotProgram _ (RobotProgramExecutionParams eqName robotName progName args)) =
            if (eqExists && robotExists && programExists)
                then []
                else "invalid execute robot program element"
            where
                eqExists = eqName `elem` (map equipmentName eqs)
                [eq] = filter ((==) eqName . equipmentName) eqs
                robots = availableRobots eq
                robotExists = robotName `elem` (map rdName robots)
                [robot] = filter ((==) robotName . rdName) robots
                programExists = progName `elem` (map rpName $ rdRobotPrograms robot)
                -- TODO: check if the program arguments make sense

-- apply f to all contained MeasurementElements and combine the results
foldMeasurementElement :: (Monoid a) => (MeasurementElement -> a) -> MeasurementElement -> a
foldMeasurementElement f me = foldMeasurementElement' f mempty me
    where
        foldMeasurementElement' :: (Monoid a) => (MeasurementElement -> a) -> a -> MeasurementElement -> a
        foldMeasurementElement' f ac m@(MEDoTimes _ _ _ es)             = mconcat (ac : f m : childVals f es)
        foldMeasurementElement' f ac m@(METimeLapse _ _ _ _ es)         = mconcat (ac : f m : childVals f es)
        foldMeasurementElement' f ac m@(MEStageLoop _ _ _ _ es)         = mconcat (ac : f m : childVals f es)
        foldMeasurementElement' f ac m@(MERelativeStageLoop _ _ _ _ es) = mconcat (ac : f m : childVals f es)
        foldMeasurementElement' f ac m = ac `mappend` (f m)
        childVals :: (Monoid a) => (MeasurementElement -> a) -> [MeasurementElement] -> [a]
        childVals f mes = map (foldMeasurementElement f) mes

-- empty list as value means no error
validateDetection :: [EquipmentW] -> DetectionParams -> [String]
validateDetection eqs (DetectionParams _ irradiationParams movableComponentParams) =
    if ((null . concat . map (validateIrradiation eqs) $ irradiationParams)
       && all (\p -> isValidMovableComponentParams eqs p) movableComponentParams)
    then []
    else ["invalid detection params"]
    where
        isValidMovableComponentParams :: [EquipmentW] -> MovableComponentParams -> Bool
        isValidMovableComponentParams eqs (MovableComponentParams eqName settings) =
            eqExists && all (\s -> isValidMovableComponentSetting eq s) settings
            where
                eqExists = eqName `elem` (map equipmentName eqs)
                eq = head (filter ((==) eqName  . equipmentName) eqs)
        isValidMovableComponentSetting eq setting =
            let compDescriptions = availableMovableComponents eq
                mcDescription = filter (\d -> componentName d == setting.mcsComponentName) compDescriptions
                componentExists = not $ null mcDescription
                component = head (mcDescription)
                typeMatches :: MovableComponentSetting -> MovableComponentDescription -> Bool
                typeMatches DiscreteComponentSetting{} DiscreteMovableComponent{} = True
                typeMatches ContinuousComponentSetting{} ContinuouslyMovableComponent{} = True
                typeMatches _ _ = False
                paramsValid :: MovableComponentSetting -> MovableComponentDescription -> Bool
                paramsValid (DiscreteComponentSetting _ setting) (DiscreteMovableComponent _ possibleSettings) =
                    setting `elem` possibleSettings
                paramsValid (ContinuousComponentSetting _ val) (ContinuouslyMovableComponent _ minVal maxVal _) =
                    (val >= minVal) && (val <= maxVal)
            in  componentExists && (typeMatches setting component) && (paramsValid setting component)

validateIrradiation :: [EquipmentW] -> IrradiationParams -> [String]
validateIrradiation eqs IrradiationParams{..} =
    case (lookupMaybeLightSource eqs (ipEquipmentName, ipLightSourceName)) of
      Nothing -> ["invalid equipment / light source name"]
      Just eq -> let errMsg = validLightSourceChannelsAndPowers eq ipLightSourceName ipLightSourceChannels ipPowers
                 in if (T.null errMsg)
                    then []
                    else ["invalid light source parameters for " ++ T.unpack (fromLSName ipLightSourceName) ++ ": " ++ T.unpack errMsg]

validLightSourceChannelsAndPowers :: EquipmentW -> LSName -> [LSChannelName] -> [LSIlluminationPower] -> Text
validLightSourceChannelsAndPowers eq lsName channels powers
    | length channels /= length powers = "must have same number of channels and powers"
    | (length channels > 1) && (not (lsdAllowsMultipleChannels lsParams)) = "light source does not allow multiple channels"
    | null channels = "channels cannot be empty"
    | not (all (\c -> lightSourceHasChannel eq c) channels) = "invalid channel(s)"
    | not (all (\(LSIlluminationPower p) -> within p 0.0 100.0) powers) = "power outside valid range"
    | not (nodups channels) = "duplicate channels requested"
    | otherwise = T.empty
    where
        lsParams = head $ filter ((lsName ==) . lsdName)  (availableLightSources eq)
        lightSourceHasChannel e c = c `elem` (lsdChannels lsParams)
