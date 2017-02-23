{-# LANGUAGE RecordWildCards #-}
module MeasurementProgramVerification where

import Data.Either
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import FilterWheel
import LightSources
import MeasurementProgramTypes
import MotorizedStage
import MiscUtils

validateMeasurementElement :: [LightSource] -> [FilterWheel] -> [MotorizedStage] -> MeasurementElement -> Either String ()
validateMeasurementElement lss fws _ (MEDetection dets)
    | null dets = Left "no detection specified"
    | otherwise = sequenceEither (map (validateDetection lss fws) dets)
validateMeasurementElement lss _ _ (MEIrradiation dur ips)
    | (dur < 0.0) || (dur > 3600) = Left ("invalid irradiation duration: " ++ show dur)
    | otherwise = sequenceEither (map (validateIrradiation lss) ips)
validateMeasurementElement _ _ _ (MEWait dur)
    | (dur < 0.0) || (dur > 3600) = Left ("invalid wait duration: " ++ show dur)
    | otherwise = Right ()
validateMeasurementElement lss fws sts (MEDoTimes n es)
    | (n < 0) || (n > (floor 10e6)) = Left ("invalid number of times to repeat: " ++ show n)
    | null es = Left "do times loop but no actions"
    | otherwise = sequenceEither (map (validateMeasurementElement lss fws sts) es)
validateMeasurementElement lss fws _ (MEFastAcquisitionLoop n det)
    | (n < 0) || (n > (floor 10e6)) = Left ("invalid number of times to repeat: " ++ show n)
    | otherwise = validateDetection lss fws det
validateMeasurementElement lss fws sts (METimeLapse n dur es)
    | (n < 0) || (n > (floor 10e6)) = Left ("invalid number of times to repeat: " ++ show n)
    | (dur < 0.0) || (dur > 3600 * 2) = Left ("invalid time lapse duration: " ++ show dur)
    | null es = Left "timelapse loop but no actions"
    | otherwise = sequenceEither (map (validateMeasurementElement lss fws sts) es)
validateMeasurementElement lss fws sts (MEStageLoop stageName pos es)
    | null pos = Left ("no positions in stage loop")
    | T.null stageName = Left ("no stage name")
    | stageName `notElem` stageNames = Left ("can't find stage named " ++ T.unpack stageName)
    | null es = Left "stage loop but no actions"
    | otherwise = sequenceEither (map (validateMeasurementElement lss fws sts) es)
    where
        stageNames = map motorizedStageName sts

validateDetection :: [LightSource] -> [FilterWheel] -> DetectionParams -> Either String ()
validateDetection lightSources filterWheels DetectionParams{..} =
    if ((within dpExposureTime 3.8e-3 10) && (within dpNSpectraToAverage 1 1000)
       && (all isRight $ map (validateIrradiation lightSources) dpIrradiation) && filtersAreValid)
    then Right ()
    else Left "invalid detection params"
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

validateIrradiation :: [LightSource] -> IrradiationParams -> Either String ()
validateIrradiation lightSources IrradiationParams{..} =
    case (lookupMaybeLightSource lightSources ipLightSourceName) of
      Nothing -> Left "invalid light source name"
      Just ls -> let errMsg = validLightSourceChannelsAndPowers ls ipLightSourceChannel ipPower
                 in if (T.null errMsg)
                    then Right ()
                    else Left ("invalid light source parameters for " ++ T.unpack ipLightSourceName ++ ": " ++ T.unpack errMsg)
