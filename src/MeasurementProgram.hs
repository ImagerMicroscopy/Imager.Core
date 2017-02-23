{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module MeasurementProgram (
    executeMeasurement
  , executeDetection
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Either
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import System.Clock

import Detector
import FilterWheel
import LightSources
import MeasurementProgramTypes
import MeasurementProgramVerification
import MotorizedStage
import MiscUtils

executeMeasurement :: Detector a => ProgramEnvironment a -> MeasurementElement -> IO (Either String ())
executeMeasurement env me = executeMeasurementElement env (insertFastAcquisitionLoops me)

executeMeasurementElements :: Detector a => ProgramEnvironment a -> [MeasurementElement] -> IO (Either String ())
executeMeasurementElements env es = (sequenceExcept . map (executeMeasurementElement env)) es

executeMeasurementElement :: Detector a => ProgramEnvironment a -> MeasurementElement -> IO (Either String ())
executeMeasurementElement env (MEDetection detParams) =
    forM detParams (\p -> executeDetection detector lss fws p) >>=
    return . map fromRight >>=
    addDataToMVar mvar startTime >>
    return (Right ())
    where
      detector = peDetector env
      lss = peLightSources env
      fws = peFilterWheels env
      mvar = peDataMVar env
      startTime = peStartTime env
executeMeasurementElement env (MEIrradiation dur ips) = executeIrradiation lss fws ips dur
    where
      lss = peLightSources env
      fws = peFilterWheels env
executeMeasurementElement _ (MEWait dur) = threadDelay (round $ dur * 1e6) >> return (Right ())
executeMeasurementElement env (MEDoTimes n es) = executeMeasurementElements env . concat . take n . repeat $ es
executeMeasurementElement env (MEFastAcquisitionLoop n detParams) =
    executeFastDetectionLoop detector lss fws detParams n startTime mvar
    where
      lss = peLightSources env
      fws = peFilterWheels env
      mvar = peDataMVar env
      startTime = peStartTime env
      detector = peDetector env
executeMeasurementElement env (METimeLapse n dur es) =
    futureMeasurements >>= \ft ->
    forM_ ft (\(ts, es) ->
        waitUntil ts >> executeMeasurementElements env es) >>
    return (Right ())
    where
        futureDurations :: [Double]
        futureDurations = map ((*) dur . fromIntegral) [0 .. (n - 1)]
        futureTimes :: IO [TimeSpec]
        futureTimes = getTime Monotonic >>= \now ->
                      return $ map (\delta -> fromNanoSecs (toNanoSecs now + round (delta * 1.0e9))) futureDurations
        futureMeasurements :: IO [(TimeSpec, [MeasurementElement])]
        futureMeasurements = futureTimes >>= \ft -> return (zip ft (repeat es))
        waitUntil :: TimeSpec -> IO ()
        waitUntil ts = getTime Monotonic >>= return . toNanoSecs . diffTimeSpec ts >>= \ns ->
                       threadDelay (fromIntegral (max (ns `div` 1000) 0))
executeMeasurementElement env (MEStageLoop sn poss es) =
    forM_ poss (\pos -> setStagePosition sn pos >> executeMeasurementElements env es) >>
    return (Right ())
    where
        stages = peMotorizedStages env
        setStagePosition :: Text -> StagePosition -> IO (Either String ())
        setStagePosition stageName pos = setStagePositionLookup stages stageName pos

insertFastAcquisitionLoops :: MeasurementElement -> MeasurementElement
insertFastAcquisitionLoops (MEDoTimes n es)
    | isSimpleDetection es = MEFastAcquisitionLoop n theDetection
    | otherwise            = MEDoTimes n (map insertFastAcquisitionLoops es)
    where
      isSimpleDetection es = not (null es) && all hasSingleDetection es && allIdentical (map detection es)
      hasSingleDetection (MEDetection [d]) = True
      hasSingleDetection _ = False
      detection (MEDetection [d]) = d
      allIdentical xs = length (nub xs) == 1
      theDetection = detection (head es)
insertFastAcquisitionLoops (METimeLapse n dur es) = METimeLapse n dur (map insertFastAcquisitionLoops es)
insertFastAcquisitionLoops (MEStageLoop n pos es) = MEStageLoop n pos (map insertFastAcquisitionLoops es)
insertFastAcquisitionLoops m = m

executeDetection :: Detector a => a -> [LightSource] -> [FilterWheel] -> DetectionParams -> IO (Either String AcquiredData)
executeDetection det lss fws DetectionParams{..} =
    runExceptT (
        ExceptT (switchToFilters fws dpFilterParams) >>
        ExceptT (enableLightSources lss dpIrradiation) >>
        ExceptT (acquireData det dpExposureTime dpGain dpNSpectraToAverage) >>= \acquiredData ->
        ExceptT (disableLightSources lss dpIrradiation) >>
        ExceptT (return $ Right acquiredData))

executeFastDetectionLoop :: Detector a => a -> [LightSource] -> [FilterWheel] -> DetectionParams -> Int -> TimeSpec -> MVar [[AcquiredData]] -> IO (Either String ())
executeFastDetectionLoop detector lightSources filterWheels detParams nTimesToPerform startTime dataMVar =
    runExceptT (
        ExceptT (switchToFilters filterWheels (dpFilterParams detParams)) >>
        ExceptT (enableLightSources lightSources (dpIrradiation detParams)) >>
        ExceptT (Right <$> acquireStreamingData detector (dpExposureTime detParams) (dpGain detParams)
                    (dpNSpectraToAverage detParams) nTimesToPerform startTime dataMVar) >>
        ExceptT (disableLightSources lightSources (dpIrradiation detParams)))

executeIrradiation :: [LightSource] -> [FilterWheel] -> [IrradiationParams] -> Double -> IO (Either String ())
executeIrradiation lightSources fws ips dur =
    runExceptT (
        ExceptT (enableLightSources lightSources ips) >>
        ExceptT (Right <$> threadDelay (floor $ dur * 1.0e6)) >>
        ExceptT (disableLightSources lightSources ips))

switchToFilters :: [FilterWheel] -> [FilterParams] -> IO (Either String ())
switchToFilters fws fps =
    mapM (\(FilterParams fwName fw) -> switchFilterWheel fws fwName fw) fps >>= \results ->
    if (any isLeft results)
    then return (head . filter isLeft $ results)
    else return (Right ())

enableLightSources :: [LightSource] -> [IrradiationParams] -> IO (Either String ())
enableLightSources lss params
    | allLightSourcesKnown = Right <$> forM_ params (\(IrradiationParams sourceName channel power) ->  activateLightSource (lookupLightSource lss sourceName) channel power)
                             --Right <$> return ()
    | otherwise = return (Left "unknown light source")
    where
        allLightSourcesKnown = and $ map (isKnownLightSource lss . ipLightSourceName) params
disableLightSources :: [LightSource] -> [IrradiationParams] -> IO (Either String ())
disableLightSources lss params = catch (Right <$> mapM_ (\(IrradiationParams sourceName _ _) -> deactivateLightSource (lookupLightSource lss sourceName)) params)
                                    (\e -> return (Left (displayException (e :: IOException))))
    where
        allLightSourcesKnown = and $ map (isKnownLightSource lss . ipLightSourceName) params
