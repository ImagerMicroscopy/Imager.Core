{-# LANGUAGE RecordWildCards #-}
module MeasurementProgramVerification (
    validateMeasurementElementThrows
  , validateMeasurementElement
  , foldMeasurementElement
) where

import Control.Exception
import Data.Either
import Data.List
import Data.Maybe
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T

import EquipmentTypes
import FilterWheel
import LightSources
import MeasurementProgramTypes
import MotorizedStage
import MiscUtils
import Robot

type RobotInfo = (Text, [Text]) -- robot name, robot programs

validateMeasurementElementThrows :: [Equipment] -> [Equipment] -> [Equipment] -> [Equipment] -> MeasurementElement -> IO ()
validateMeasurementElementThrows lss fws mss rss me =
    verifyRobotElements rss me >>= \robotsResult ->
    when (not $ null robotsResult) (
        throwIO (userError $ head robotsResult)) >>
    case (foldMeasurementElement (validateMeasurementElement lss fws mss rss) me) of
                                                          (e : xs) -> throwIO (userError e)
                                                          []       -> return ()

-- should not inspect contained MeasurementElements
-- empty list for no error
validateMeasurementElement :: [Equipment] -> [Equipment] -> [Equipment] -> [Equipment] -> MeasurementElement -> [String]
validateMeasurementElement lss fws _ _ (MEDetection dets)
    | null dets = ["no detection specified"]
    | otherwise = concat $ (map (validateDetection lss fws) dets)
validateMeasurementElement lss _ _ _ (MEIrradiation dur ips)
    | (dur < 0.0) || (dur > 3600) = ["invalid irradiation duration: " ++ show dur]
    | otherwise = concat $ (map (validateIrradiation lss) ips)
validateMeasurementElement _ _ _ _ (MEWait dur)
    | (dur < 0.0) || (dur > 3600) = ["invalid wait duration: " ++ show dur]
    | otherwise = []
validateMeasurementElement _ _ _ rss (MEExecuteRobotProgram rName pName _) = []
validateMeasurementElement lss fws sts rss (MEDoTimes n es)
    | (n < 0) || (n > (floor 10e6)) = ["invalid number of times to repeat: " ++ show n]
    | null es = ["do times loop but no actions"]
    | otherwise = []
validateMeasurementElement lss fws _ _ (MEFastAcquisitionLoop n det)
    | (n < 0) || (n > (floor 10e6)) = ["invalid number of times to repeat: " ++ show n]
    | otherwise = validateDetection lss fws det
validateMeasurementElement lss fws sts rss (METimeLapse n dur es)
    | (n < 0) || (n > (floor 10e6)) = ["invalid number of times to repeat: " ++ show n]
    | (dur < 0.0) || (dur > 3600 * 2) = ["invalid time lapse duration: " ++ show dur]
    | null es = ["timelapse loop but no elements"]
    | otherwise = []
validateMeasurementElement lss fws sts rss (MEStageLoop stageName pos es)
    | null pos = ["no positions in stage loop"]
    | T.null stageName = ["no stage name"]
    | stageName `notElem` stageNames = ["can't find stage named " ++ T.unpack stageName]
    | null es = ["stage loop but no elements"]
    | otherwise = []
    where
        stageNames = map motorizedStageName sts
validateMeasurementElement lss fws sts rss (MERelativeStageLoop stageName (RelativeStageLoopParams dx dy dz (bx, ax) (by, ay) (bz, az)) es)
    | T.null stageName = ["no stage name"]
    | stageName `notElem` stageNames = ["can't find stage named " ++ T.unpack stageName]
    | null es = ["relative stage loop but no elements"]
    | (dx < 0) || (dy < 0) || (dz < 0) = ["invalid additional planes distance"]
    | (bx < 0) || (ax < 0) = ["invalid additional planes x"]
    | (by < 0) || (ay < 0) = ["invalid additional planes y"]
    | (bz < 0) || (az < 0) = ["invalid additional planes z"]
    | otherwise = []
    where
        stageNames = map motorizedStageName sts

verifyRobotElements :: [Equipment] -> MeasurementElement -> IO [String]
verifyRobotElements  rss me
        = do
          pExists <- programsExist
          rAllows <- robotsAllowExecution
          if (not pExists)
          then return ["referring to unknown program"]
          else if (not rAllows)
               then return ["robot does not allow remote execution"]
               else return []
    where
        availableRobotNames = map robotName rss
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
validateDetection :: [Equipment] -> [Equipment] -> DetectionParams -> [String]
validateDetection lightSources filterWheels DetectionParams{..} =
    if ((within dpExposureTime 1e-3 10) && (within dpNSpectraToAverage 1 1000)
       && (null . concat . map (validateIrradiation lightSources) $ dpIrradiation)
       && filtersAreValid)
    then []
    else ["invalid detection params"]
    where
        filterParams = dpFilterParams
        filtersAreValid = noEmptyFilterWheelNames && noEmptyFilterNames && noDupFilterWheels && all filterExists filterParams
        noEmptyFilterWheelNames = all (not . T.null) (map fpFilterWheelName filterParams)
        noEmptyFilterNames = all (not . T.null) (map fpFilterName filterParams)
        noDupFilterWheels = nodups (map fpFilterWheelName filterParams)
        filterExists (FilterParams fwName fName) =
            case (lookup fwName (map (\f -> (filterWheelName f, filterWheelChannels f)) filterWheels)) of
                Nothing -> False
                Just (availableFilters) -> fName `elem` availableFilters

validateIrradiation :: [Equipment] -> IrradiationParams -> [String]
validateIrradiation lightSources IrradiationParams{..} =
    case (lookupMaybeLightSource lightSources ipLightSourceName) of
      Nothing -> ["invalid light source name"]
      Just ls -> let errMsg = validLightSourceChannelsAndPowers ls ipLightSourceChannel ipPower
                 in if (T.null errMsg)
                    then []
                    else ["invalid light source parameters for " ++ T.unpack ipLightSourceName ++ ": " ++ T.unpack errMsg]
