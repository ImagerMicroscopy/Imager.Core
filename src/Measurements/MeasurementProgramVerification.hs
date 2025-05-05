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
    fmap ((++) (validateDefinedDetections dets ddets)) (verifyRobotElements eqs me) >>= \result ->
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
validateMeasurementElement eqs ddets (MEDetection detNames)
    | null detNames = ["no detection specified"]
    | any (\dn -> not (M.member dn ddets)) detNames = ["one or more detection names are undefined"]
    | otherwise = concat $ (map (validateDetection eqs) dets)
    where
        dets = map (\dn -> fromJust $ M.lookup dn ddets) detNames
validateMeasurementElement eqs _ (MEIrradiation dur ips)
    | (fromLSIlluminationDuration dur < 0.0) || (fromLSIlluminationDuration dur > 60) = ["invalid irradiation duration: " ++ show dur]
    | otherwise = concat $ (map (validateIrradiation eqs) ips)
validateMeasurementElement eqs _ (MEWait dur)
    | (dur < 0.0) || (dur > 3600) = ["invalid wait duration: " ++ show dur]
    | otherwise = []
validateMeasurementElement eqs _ (MEExecuteRobotProgram rName pName _) = []
validateMeasurementElement eqs _ (MEDoTimes n es)
    | (n < 0) || (n > (floor 10e6)) = ["invalid number of times to repeat: " ++ show n]
    | null es = ["do times loop but no actions"]
    | otherwise = []
validateMeasurementElement eqs _ (MEFastAcquisitionLoop n (detName, detParams))
    | (n < 0) || (n > (floor 10e6)) = ["invalid number of times to repeat: " ++ show n]
    | otherwise = validateDetection eqs detParams
validateMeasurementElement eqs _ (METimeLapse n dur es)
    | (n < 0) || (n > (floor 10e6)) = ["invalid number of times to repeat: " ++ show n]
    | (dur < 0.0) || (dur > 3600 * 2) = ["invalid time lapse duration: " ++ show dur]
    | null es = ["timelapse loop but no elements"]
    | otherwise = []
validateMeasurementElement eqs _ (MEStageLoop stageName pos es)
    | null pos = ["no positions in stage loop"]
    | T.null (fromStageName stageName) = ["no stage name"]
    | stageName `notElem` stageNames = ["can't find stage named " ++ T.unpack (fromStageName stageName)]
    | null es = ["stage loop but no elements"]
    | otherwise = []
    where
        stageNames = map motorizedStageName (filter hasMotorizedStage eqs)
validateMeasurementElement eqs _ (MERelativeStageLoop stageName (RelativeStageLoopParams dx dy dz (bx, ax) (by, ay) (bz, az) _) es)
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

verifyRobotElements :: [EquipmentW] -> MeasurementElement -> IO [String]
verifyRobotElements  eqs me = do
          pExists <- programsExist
          rAllows <- robotsAllowExecution
          if (not pExists)
          then return ["referring to unknown program"]
          else if (not rAllows)
               then return ["robot does not allow remote execution"]
               else return []
    where
        rss = filter hasRobot eqs
        availableRobotNames = map equipmentName rss
        robotsAndProgramsInProgram = map (\(MEExecuteRobotProgram rName pName _) -> (lookupRobotThrows rss rName, pName)) robotElements
        usedRobots = map fst robotsAndProgramsInProgram
        robotsAllowExecution = and <$> mapM robotAcceptsExternalCommands usedRobots
        programsExist = and <$> (mapM (\(robot, pName) -> (pName `elem`) <$> listRobotPrograms robot) robotsAndProgramsInProgram)
        robotElements = foldMeasurementElement f me
        f m@(MEExecuteRobotProgram _ _ _) = [m]
        f _ = []

-- apply f to all contained MeasurementElements and combine the results
foldMeasurementElement :: (Monoid a) => (MeasurementElement -> a) -> MeasurementElement -> a
foldMeasurementElement f me = foldMeasurementElement' f mempty me
    where
        foldMeasurementElement' :: (Monoid a) => (MeasurementElement -> a) -> a -> MeasurementElement -> a
        foldMeasurementElement' f ac m@(MEDoTimes _ es)             = mconcat (ac : f m : childVals f es)
        foldMeasurementElement' f ac m@(METimeLapse _ _ es)         = mconcat (ac : f m : childVals f es)
        foldMeasurementElement' f ac m@(MEStageLoop _ _ es)         = mconcat (ac : f m : childVals f es)
        foldMeasurementElement' f ac m@(MERelativeStageLoop _ _ es) = mconcat (ac : f m : childVals f es)
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
                paramsValid (ContinuousComponentSetting _ val) (ContinuouslyMovableComponent _ minVal maxVal) =
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
