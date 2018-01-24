{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}
module MeasurementProgram (
    executeMeasurement
  , executeDetection
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.DeepSeq
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
import System.Mem
import System.Clock
import qualified System.Timeout as ST

import CameraImageProcessing
import Detector
import Equipment
import EquipmentTypes
import MeasurementProgramTypes
import MeasurementProgramVerification
import MeasurementProgramUtils
import MiscUtils

executeMeasurement :: Detector a => ProgramEnvironment a -> MeasurementElement -> IO ()
executeMeasurement env me = withAsync (forever $ resetSystemSleepTimer >> threadDelay (round 60.0e6)) (\_ ->
                                executeMeasurementElement env (insertFastAcquisitionLoops me)
                                `catch` (\e -> deactivateAllLightSources usedLightSources >>
                                               mapM_ abortRobotProgramExecution usedRobots >>
                                               putStrLn (displayException e) >>
                                               throwIO (e :: SomeException)))
    where
        lss = peLightSources env
        robots = peRobots env
        usedLightSourceNames = lightSourceNamesUsedIn me
        usedLightSources = map (lookupLightSource lss) usedLightSourceNames
        usedRobots = map (lookupRobotThrows robots) (robotNamesUsedIn me)

executeMeasurementElements :: Detector a => ProgramEnvironment a -> [MeasurementElement] -> IO ()
executeMeasurementElements env es = mapM_ (executeMeasurementElement env) es

executeMeasurementElement :: Detector a => ProgramEnvironment a -> MeasurementElement -> IO ()
executeMeasurementElement env (MEDetection detParams) =
    forM_ detParams (\p -> executeDetection detector lss fws p >>=
                           return . rearrangeImageExternal rearrangeFuncs >>= \r ->
                           r `deepseq` addDataToMVar mvar startTime r)
    where
      detector = peDetector env
      lss = peLightSources env
      fws = peFilterWheels env
      rearrangeFuncs = peRearrangementFuncs env
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
        executeFastDetectionLoop detector lss fws detParams rearrangeFuncs n startTime mvar)
    where
      lss = peLightSources env
      fws = peFilterWheels env
      rearrangeFuncs = peRearrangementFuncs env
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
            setStagePosition stage pos >> executeMeasurementElements env es))
    where
        stage = lookupStageThrows (peMotorizedStages env) sn
        nPos = length poss

executeMeasurementElement env (MERelativeStageLoop sn (RelativeStageLoopParams dx dy dz (bx, ax) (by, ay) (bz, az)) es) =
    withStatusMessage env "relative stage loop" (
        getStagePosition stage >>= \currPos ->
        let poss = (allPositions currPos)
        in  forM_ (zip [1..] (allPositions currPos)) (\(index :: Int, pos) ->
                updateStatusMessage env (T.format "relative stage position {} of {}" (index, length poss)) >>
                setStagePosition stage pos >> executeMeasurementElements env es))
    where
        stage = lookupStageThrows (peMotorizedStages env) sn
        planesx = map ((*) dx . fromIntegral) [negate bx .. ax] :: [Double]
        planesy = map ((*) dy . fromIntegral) [negate by .. ay]
        planesz = map ((*) dz . fromIntegral) [negate bz .. az]
        allPositions :: StagePosition -> [StagePosition]
        allPositions (x, y, z) = [(x + x', y + y', z + z') | x' <- planesx, y' <- planesy, z' <- planesz]

insertFastAcquisitionLoops :: MeasurementElement -> MeasurementElement
insertFastAcquisitionLoops (MEDoTimes n [MEDetection [d]]) = MEFastAcquisitionLoop n d
insertFastAcquisitionLoops (MEDoTimes n es) = MEDoTimes n (map insertFastAcquisitionLoops es)
insertFastAcquisitionLoops (METimeLapse n dur es) = METimeLapse n dur (map insertFastAcquisitionLoops es)
insertFastAcquisitionLoops (MEStageLoop n pos es) = MEStageLoop n pos (map insertFastAcquisitionLoops es)
insertFastAcquisitionLoops m = m

executeDetection :: Detector a => a -> [EquipmentW] -> [EquipmentW] -> DetectionParams -> IO AcquiredData
executeDetection det lss fws DetectionParams{..} =
    setBinningFactor det dpBinningFactor >>
    setCropSize det dpCropSize >>
    switchToFilters fws dpFilterParams >>
    enableLightSources lss dpIrradiation >>
    acquireData det dpExposureTime dpGain dpNSpectraToAverage >>= \acquiredData ->
    disableLightSources lss dpIrradiation >>
    return acquiredData

executeFastDetectionLoop :: Detector a => a -> [EquipmentW] -> [EquipmentW] -> DetectionParams -> [ExternalRearrangementFunc] -> Int -> TimeSpec -> MVar [AcquiredData] -> IO ()
executeFastDetectionLoop detector lightSources filterWheels detParams rearrangeFuncs nTimesToPerform startTime dataMVar =
    setBinningFactor detector (dpBinningFactor detParams) >>
    setCropSize detector (dpCropSize detParams) >>
    switchToFilters filterWheels (dpFilterParams detParams) >>
    enableLightSources lightSources (dpIrradiation detParams) >>
    newChan >>= \chan ->
    withAsync (acquireStreamingData detector (dpExposureTime detParams) (dpGain detParams)
                    (dpNSpectraToAverage detParams) nTimesToPerform chan) (\as ->
        fetchData 0 as chan) >>
    disableLightSources lightSources (dpIrradiation detParams)
    where
        fetchData :: Int -> Async () -> Chan AsyncData -> IO ()
        fetchData nFetched as chan = readChan chan >>= \val ->
                                     case val of
                                         AsyncFinished -> performMajorGC >> wait as
                                         AsyncError    -> performMajorGC >> wait as
                                         AsyncData d   -> let r = rearrangeImageExternal rearrangeFuncs d
                                                          in  when (nFetched `mod` 50 == 49) (performMajorGC) >>
                                                              r `deepseq` addDataToMVar dataMVar startTime r >>
                                                              fetchData (nFetched + 1) as chan

executeIrradiation :: [EquipmentW] -> [EquipmentW] -> [IrradiationParams] -> Double -> IO ()
executeIrradiation lightSources fws ips dur =
    enableLightSources lightSources ips >>
    when (dur > 0.0) (threadDelay (floor $ dur * 1.0e6)) >>
    disableLightSources lightSources ips

switchToFilters :: [EquipmentW] -> [FilterParams] -> IO ()
switchToFilters fws fps =
    forM_ fps (\(FilterParams fwName fw) -> switchFilterWheel fws fwName fw)

enableLightSources :: [EquipmentW] -> [IrradiationParams] -> IO ()
enableLightSources lss params =
    forM_ params (\(IrradiationParams sourceName channel power) ->  activateLightSource (lookupLightSource lss sourceName) channel power)

disableLightSources :: [EquipmentW] -> [IrradiationParams] -> IO ()
disableLightSources lss params =
    forM_ params (\(IrradiationParams sourceName _ _) -> deactivateLightSource (lookupLightSource lss sourceName))

deactivateAllLightSources :: [EquipmentW] -> IO ()
deactivateAllLightSources sources = mapM_ deactivateLightSource sources

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

switchFilterWheel :: [EquipmentW] -> Text -> Text -> IO ()
switchFilterWheel fws fwName fName =
    let filterWheel = head (filter (\fw -> hasFilterWheel fw && (filterWheelName fw == fwName)) fws)
    in ST.timeout (floor 10e6) (switchToFilter filterWheel fName) >>= \result ->
       case result of
           Nothing -> throwIO (userError ("timeout communicating with " ++ T.unpack (filterWheelName filterWheel)))
           Just v -> return v

robotNamesUsedIn :: MeasurementElement -> [Text]
robotNamesUsedIn = foldMeasurementElement f
    where
        f :: MeasurementElement -> [Text]
        f (MEExecuteRobotProgram rName _ _) = [rName]
        f _ = []

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

addDataToMVar :: MVar [AcquiredData] -> TimeSpec -> AcquiredData -> IO ()
addDataToMVar dataMVar startTime d = modifyMVar_ dataMVar (\previousData ->
                                 when (length previousData > 250) (throwIO (userError "too many async data stored")) >>
                                 if (null previousData)
                                 then pure [adjustTime d]
                                 else pure (adjustTime d : previousData))
    where
       adjustTime :: AcquiredData -> AcquiredData
       adjustTime acqDat = let t = acqTimeStamp acqDat
                           in acqDat {acqTimeStamp = diffTimeSpec t startTime}
