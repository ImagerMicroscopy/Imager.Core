{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}
module MeasurementProgram (
    executeMeasurement
  , executeDetection
  , robotProgramsUsedIn
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Either
import Data.List
import Data.Monoid
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Format as T
import Data.Time.Clock
import Data.Time.LocalTime
import System.Clock

import Detector
import FilterWheel
import LightSources
import MeasurementProgramTypes
import MeasurementProgramVerification
import MotorizedStage
import MiscUtils
import Robot

executeMeasurement :: Detector a => ProgramEnvironment a -> MeasurementElement -> IO ()
executeMeasurement env me = withAsync (forever $ resetSystemSleepTimer >> threadDelay (round 60.0e6)) (\_ ->
                                executeMeasurementElement env (insertFastAcquisitionLoops me)
                                `onException` (deactivateAllLightSources usedLightSources >>
                                               mapM_ abortRobotProgramExecution usedRobots))
    where
        lss = peLightSources env
        robots = peRobots env
        usedLightSourceNames = lightSourceNamesUsedIn me
        usedLightSources = map (lookupLightSource lss) usedLightSourceNames
        usedRobotNames = map fst (robotProgramsUsedIn me)
        usedRobots = map (lookupRobotThrows robots) usedRobotNames

executeMeasurementElements :: Detector a => ProgramEnvironment a -> [MeasurementElement] -> IO ()
executeMeasurementElements env es = mapM_ (executeMeasurementElement env) es

executeMeasurementElement :: Detector a => ProgramEnvironment a -> MeasurementElement -> IO ()
executeMeasurementElement env (MEDetection detParams) =
    forM detParams (\p -> executeDetection detector lss fws p) >>=
    addDataToMVar mvar startTime
    where
      detector = peDetector env
      lss = peLightSources env
      fws = peFilterWheels env
      mvar = peDataMVar env
      startTime = peStartTime env
executeMeasurementElement env (MEIrradiation dur ips) =
    withStatusMessage env (T.format "irradiating {} s" (T.Only dur)) (
        executeIrradiation lss fws ips dur)
    where
      lss = peLightSources env
      fws = peFilterWheels env
executeMeasurementElement env (MEWait dur) =
    withStatusMessage env (T.format "waiting {} s" (T.Only dur)) (
        threadDelay (round $ dur * 1e6))
executeMeasurementElement env (MEExecuteRobotProgram rName pName wait) =
    withStatusMessage env (T.format "executing program {} on {}" (pName, rName)) (
        executeRobotProgram (lookupRobotThrows robots rName) pName wait)
    where
        robots = peRobots env
executeMeasurementElement env (MEDoTimes n es) =
    withStatusMessage env "do times" (
        forM_ (zip [1 ..] (take n . repeat $ es)) (\(index :: Int, ses) ->
            updateStatusMessage env (T.format "do times {} of {}" (index, n)) >>
            executeMeasurementElements env ses
        ))
executeMeasurementElement env (MEFastAcquisitionLoop n detParams) =
    withStatusMessage env (T.format "fast acquisition ({} images)" (T.Only n)) (
        executeFastDetectionLoop detector lss fws detParams n startTime mvar)
    where
      lss = peLightSources env
      fws = peFilterWheels env
      mvar = peDataMVar env
      startTime = peStartTime env
      detector = peDetector env
executeMeasurementElement env (METimeLapse n dur es) =
    futureTimes >>= \fts ->
    timeSpecToUTCTimes fts >>= \utcs ->
    withStatusMessage env "time lapse" (
        forM_ (zip3 [1 ..] fts utcs) (\(index :: Int, ts, utc) ->
            formattedTime utc >>= \timeStr ->
            updateStatusMessage env (T.format "next time lapse ({} of {}) at {}" (index, n, timeStr)) >>
            waitUntil ts >>
            updateStatusMessage env (T.format "executing time lapse {} of {}" (index, n)) >>
            executeMeasurementElements env es))
    where
        futureDurations = map ((*) dur . fromIntegral) [0 .. (n - 1)]
        futureTimes :: IO [TimeSpec]
        futureTimes = getTime Monotonic >>= \now ->
                      return $ map (\delta -> fromNanoSecs (toNanoSecs now + round (delta * 1.0e9))) futureDurations
        timeSpecToUTCTimes :: [TimeSpec] -> IO [UTCTime]
        timeSpecToUTCTimes tss = getTime Monotonic >>= \now ->
                                 getCurrentTime >>= \nowUTC ->
                                 return (map (\ts ->
                                   let picosecs = toNanoSecs (diffTimeSpec ts now) * 1000
                                       psDiff = picosecondsToDiffTime picosecs
                                   in addUTCTime (realToFrac psDiff) nowUTC) tss)
        formattedTime :: UTCTime -> IO Text
        formattedTime utc = getCurrentTimeZone >>= \tz ->
                            let lt = utcToLocalTime tz utc
                                tod = localTimeOfDay lt
                            in (return . T.pack . take 8 . show) tod
        waitUntil :: TimeSpec -> IO ()
        waitUntil ts = getTime Monotonic >>= \now ->
                       if (now <= ts)
                       then let ns = toNanoSecs $ diffTimeSpec ts now
                            in threadDelay (fromIntegral (max (ns `div` 1000) 0))
                       else return ()
executeMeasurementElement env (MEStageLoop sn poss es) =
    withStatusMessage env "stage loop" (
        forM_ (zip [1..] poss) (\(index :: Int, (PositionNameAndCoords posName pos)) ->
            updateStatusMessage env (T.format "stage position {} of {} ({})" (index, nPos, posName)) >>
            setStagePosition sn pos >> executeMeasurementElements env es))
    where
        stages = peMotorizedStages env
        nPos = length poss
        setStagePosition :: Text -> StagePosition -> IO ()
        setStagePosition stageName pos = setStagePositionLookup stages stageName pos

insertFastAcquisitionLoops :: MeasurementElement -> MeasurementElement
insertFastAcquisitionLoops (MEDoTimes n [MEDetection [d]]) = MEFastAcquisitionLoop n d
insertFastAcquisitionLoops (MEDoTimes n es) = MEDoTimes n (map insertFastAcquisitionLoops es)
insertFastAcquisitionLoops (METimeLapse n dur es) = METimeLapse n dur (map insertFastAcquisitionLoops es)
insertFastAcquisitionLoops (MEStageLoop n pos es) = MEStageLoop n pos (map insertFastAcquisitionLoops es)
insertFastAcquisitionLoops m = m

executeDetection :: Detector a => a -> [LightSource] -> [FilterWheel] -> DetectionParams -> IO AcquiredData
executeDetection det lss fws DetectionParams{..} =
    switchToFilters fws dpFilterParams >>
    enableLightSources lss dpIrradiation >>
    acquireData det dpExposureTime dpGain dpNSpectraToAverage >>= \acquiredData ->
    disableLightSources lss dpIrradiation >>
    return acquiredData

executeFastDetectionLoop :: Detector a => a -> [LightSource] -> [FilterWheel] -> DetectionParams -> Int -> TimeSpec -> MVar [[AcquiredData]] -> IO ()
executeFastDetectionLoop detector lightSources filterWheels detParams nTimesToPerform startTime dataMVar =
    switchToFilters filterWheels (dpFilterParams detParams) >>
    enableLightSources lightSources (dpIrradiation detParams) >>
    acquireStreamingData detector (dpExposureTime detParams) (dpGain detParams)
                    (dpNSpectraToAverage detParams) nTimesToPerform startTime dataMVar >>
    disableLightSources lightSources (dpIrradiation detParams)

executeIrradiation :: [LightSource] -> [FilterWheel] -> [IrradiationParams] -> Double -> IO ()
executeIrradiation lightSources fws ips dur =
    enableLightSources lightSources ips >>
    when (dur > 0.0) (threadDelay (floor $ dur * 1.0e6)) >>
    disableLightSources lightSources ips

switchToFilters :: [FilterWheel] -> [FilterParams] -> IO ()
switchToFilters fws fps =
    forM_ fps (\(FilterParams fwName fw) -> switchFilterWheel fws fwName fw)

enableLightSources :: [LightSource] -> [IrradiationParams] -> IO ()
enableLightSources lss params =
    forM_ params (\(IrradiationParams sourceName channel power) ->  activateLightSource (lookupLightSource lss sourceName) channel power)

disableLightSources :: [LightSource] -> [IrradiationParams] -> IO ()
disableLightSources lss params =
    forM_ params (\(IrradiationParams sourceName _ _) -> deactivateLightSource (lookupLightSource lss sourceName))

lightSourceNamesUsedIn :: MeasurementElement -> [Text]
lightSourceNamesUsedIn me = S.toList (lightSourceNamesUsedIn' S.empty me)
    where
        lightSourceNamesUsedIn' :: Set Text -> MeasurementElement -> Set Text
        lightSourceNamesUsedIn' s (MEDetection dps) = s <> (S.fromList . concat $ map (map ipLightSourceName . dpIrradiation) dps)
        lightSourceNamesUsedIn' s (MEIrradiation _ ips) = s <> ((S.fromList . map ipLightSourceName) ips)
        lightSourceNamesUsedIn' s (MEDoTimes _ mes) = s <> mconcat (map (lightSourceNamesUsedIn' S.empty) mes)
        lightSourceNamesUsedIn' s (MEFastAcquisitionLoop _ dp) = s <> S.fromList (map ipLightSourceName . dpIrradiation $ dp)
        lightSourceNamesUsedIn' s (METimeLapse _ _ mes) = s <> mconcat (map (lightSourceNamesUsedIn' S.empty) mes)
        lightSourceNamesUsedIn' s (MEStageLoop _ _ mes) = s <> mconcat (map (lightSourceNamesUsedIn' S.empty) mes)
        lightSourceNamesUsedIn' s _ = s

robotProgramsUsedIn :: MeasurementElement -> [(Text, [Text])]
robotProgramsUsedIn me = M.toList (robotProgramsUsedIn' M.empty me)
    where
        robotProgramsUsedIn' :: Map Text [Text] -> MeasurementElement -> Map Text [Text]
        robotProgramsUsedIn' m (MEExecuteRobotProgram robotName progName _) =  M.insertWith (++) robotName [progName] m
        robotProgramsUsedIn' m (MEDoTimes _ mes) = M.unionWith (++) m (M.unionsWith (++) (map (robotProgramsUsedIn' M.empty) mes))
        robotProgramsUsedIn' m (METimeLapse _ _ mes) = M.unionWith (++) m (M.unionsWith (++) (map (robotProgramsUsedIn' M.empty) mes))
        robotProgramsUsedIn' m (MEStageLoop _ _ mes) = M.unionWith (++) m (M.unionsWith (++) (map (robotProgramsUsedIn' M.empty) mes))
        robotProgramsUsedIn' m _ = m

withStatusMessage :: ProgramEnvironment a -> LT.Text -> IO b -> IO b
withStatusMessage env msg action =
    bracket (addStatusMessage msg statusMVar) (\_ -> removeStatusMessage statusMVar) (\_ -> action)
    where
        statusMVar = peStatusMVar env
        addStatusMessage :: LT.Text -> MVar [Text] -> IO ()
        addStatusMessage msg mvar =
            modifyMVar_ mvar (\ms -> return (LT.toStrict msg : ms))
        removeStatusMessage :: MVar [Text] -> IO ()
        removeStatusMessage mvar =
            modifyMVar_ mvar (\ms -> return (drop 1 ms))

updateStatusMessage :: ProgramEnvironment a -> LT.Text -> IO ()
updateStatusMessage env newMsg =
    modifyMVar_ statusMVar (\(m : ms) -> return (LT.toStrict newMsg : ms))
    where
        statusMVar = peStatusMVar env
