{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings, NumDecimals #-}
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
import Data.IORef
import Data.List
import Data.Maybe
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
import Data.Word
import System.Mem
import System.Clock
import qualified System.Timeout as ST

import AcquiredDataTypes
import Detector
import Equipment
import EquipmentTypes
import MeasurementProgramTypes
import MeasurementProgramVerification
import MeasurementProgramUtils
import MiscUtils
import SCCamDetector

executeMeasurement :: Detector a => ProgramEnvironment a -> MeasurementElement -> DefinedDetections -> IO ()
executeMeasurement env me ddets =
    withAsync (forever $ resetSystemSleepTimer >> threadDelay (round 60.0e6)) (\_ ->
        forM_ eqs (flushSerialPorts) >>
        forM_ (M.toList commonDetectorProperties) (\(detName, opts) ->
            mapM_ (setDetectorOption (namedDetector detName)) opts) >>
        executeMeasurementElement env ddetsWithoutCommon (insertFastAcquisitionLoops ddetsWithoutCommon me)
        `catch` (\e -> deactivateUsedLightSources >>
                       mapM_ abortRobotProgramExecution usedRobots >>
                       putStrLn (displayException e) >>
                       throwIO (e :: SomeException)))
    where
        detectors = peDetectors env
        namedDetector n = head (filter ((==) n . detectorName) detectors)
        eqs = peEquipment env
        eqsUsedAsLightSource = eqNamesUsedAsLightSourceIn ddets me
        deactivateUsedLightSources = mapM_ deactivateLightSource (filter (\e -> equipmentName e `elem` eqsUsedAsLightSource) eqs)
        usedRobots = let usedRobotEqNames = robotNamesUsedIn me
                     in  filter (\e -> (robotName e) `elem` usedRobotEqNames) eqs
        (ddetsWithoutCommon, commonDetectorProperties) =  removeCommonDetectorProperties ddets

removeCommonDetectorProperties :: DefinedDetections -> (DefinedDetections, Map Text [DetectorProperty])
removeCommonDetectorProperties ddets = (ddetsWithoutCommon, commonDetectorProperties)
    where
        allDetectorParams :: [DetectorParams]
        allDetectorParams = mconcat (map dpDetectors (M.elems ddets))
        allDetectorProperties :: Map Text [[DetectorProperty]]
        allDetectorProperties = foldr (\dp accum -> M.insertWith (++) (dtpDetectorName dp) [(dtpDetectorProperties dp)] accum) M.empty allDetectorParams
        commonDetectorProperties :: Map Text [DetectorProperty]
        commonDetectorProperties = M.map extractCommonDetectorProperties allDetectorProperties
            where
              extractCommonDetectorProperties :: [[DetectorProperty]] -> [DetectorProperty]
              extractCommonDetectorProperties pss =
                  let uniqueProperties = nub (mconcat pss)
                      commonProperties = filter (\prop -> all (prop `elem`) pss) uniqueProperties
                  in  commonProperties
        ddetsWithoutCommon = M.map (\detParams -> detParams {dpDetectors = removeCommon commonDetectorProperties (dpDetectors detParams)}) ddets
        removeCommon :: Map Text [DetectorProperty] -> [DetectorParams] -> [DetectorParams]
        removeCommon commonmap dtorparams =
            map (\(DetectorParams name opts) -> DetectorParams name (filter (`notElem` (fromJust $ M.lookup name commonmap)) opts)) dtorparams

executeMeasurementElements :: Detector a => ProgramEnvironment a -> DefinedDetections -> [MeasurementElement] -> IO ()
executeMeasurementElements env ddets es = mapM_ (executeMeasurementElement env ddets) es

executeMeasurementElement :: Detector a => ProgramEnvironment a -> DefinedDetections -> MeasurementElement -> IO ()
executeMeasurementElement env ddets (MEDetection detNames) =
    withStatusMessage env "Detection" (
        executeDetection detectors eqs (detNames, detParams) startTime counter mvar)
    where
      eqs = peEquipment env
      detectors = peDetectors env
      mvar = peDataMVar env
      startTime = peStartTime env
      counter = peDataCounter env
      detParams = map (\dn -> fromJust $ M.lookup dn ddets) detNames
executeMeasurementElement env _ (MEIrradiation dur ips) =
    withStatusMessage env (T.format "irradiating {} s" (T.Only (fromLSIlluminationDuration dur))) (
        executeIrradiation eqs ips dur)
    where
      eqs = peEquipment env
executeMeasurementElement env _ (MEWait dur) =
    withStatusMessage env (T.format "waiting {} s" (T.Only dur)) (
        threadDelay (round $ dur * 1e6))
executeMeasurementElement env _ (MEExecuteRobotProgram rName pName wait) =
    withStatusMessage env (T.format "executing program {} on {}" ((fromRobotProgramName pName), fromRobotName rName)) (
        executeRobotProgram robot pName wait)
    where
        [robot] = filter (\e -> hasRobot e && robotName e == rName) (peEquipment env)
executeMeasurementElement env ddets (MEDoTimes n es) =
    withStatusMessage env "do times" (
        forM_ (zip [1 ..] (take n . repeat $ es)) (\(index :: Int, ses) ->
            updateStatusMessage env (T.format "do times {} of {}" (index, n)) >>
            executeMeasurementElements env ddets ses
        ))
executeMeasurementElement env ddets (MEFastAcquisitionLoop n (detName, detParams)) =
    withStatusMessage env (T.format "fast acquisition ({} images)" (T.Only n)) (
        executeFastDetectionLoop detectors eqs (detName, detParams) n startTime counter mvar)
    where
      eqs = peEquipment env
      mvar = peDataMVar env
      startTime = peStartTime env
      detectors = peDetectors env
      counter = peDataCounter env
executeMeasurementElement env ddets (METimeLapse n dur es) =
    futureTimes >>= \fts ->
    timeSpecToUTCTimes fts >>= \utcs ->
    withStatusMessage env "time lapse" (
        forM_ (zip3 [1 ..] fts utcs) (\(index :: Int, ts, utc) ->
            formattedTime utc >>= \timeStr ->
            updateStatusMessage env (T.format "next time lapse ({} of {}) at {}" (index, n, timeStr)) >>
            waitUntil ts >>
            updateStatusMessage env (T.format "executing time lapse {} of {}" (index, n)) >>
            executeMeasurementElements env ddets es))
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
executeMeasurementElement env ddets (MEStageLoop sn poss es) =
    withStatusMessage env "stage loop" (
        forM_ (zip [1..] poss) (\(index :: Int, (PositionNameAndCoords posName pos)) ->
            updateStatusMessage env (T.format "stage position {} of {} ({})" (index, nPos, posName)) >>
            setStagePosition stageEq pos >> executeMeasurementElements env ddets es))
    where
        [stageEq] = filter (\e -> hasMotorizedStage e && motorizedStageName e == sn) (peEquipment env)
        nPos = length poss

executeMeasurementElement env ddets (MERelativeStageLoop sn (RelativeStageLoopParams dx dy dz (bx, ax) (by, ay) (bz, az)) es) =
    withStatusMessage env "relative stage loop" (
        getStagePosition stageEq >>= \(StagePosition startX startY startZ usingAF afOffset) ->
        let xCoords = map ((+) startX . (*) dx . fromIntegral) [negate bx .. ax]
            yCoords = map ((+) startY . (*) dy . fromIntegral) [negate by .. ay]
        in  forM_ xCoords (\ x->
                forM_ yCoords (\y ->
                    setStagePosition stageEq (StagePosition x y startZ usingAF afOffset) >>
                    getStagePosition stageEq >>= \(StagePosition upX upY upZ _ _) ->
                    let zCoords = map ((+) upZ . (*) dz . fromIntegral) [negate bz .. az]
                    in  forM_ zCoords (\z ->
                            setStagePosition stageEq (StagePosition upX upY z False 0) >>
                            executeMeasurementElements env ddets es
                    ))))
    where
        [stageEq] = filter (\e -> hasMotorizedStage e && motorizedStageName e == sn) (peEquipment env)

insertFastAcquisitionLoops :: DefinedDetections -> MeasurementElement -> MeasurementElement
insertFastAcquisitionLoops ddets (MEDoTimes n [MEDetection [dName]]) = MEFastAcquisitionLoop n (dName, fromJust $ M.lookup dName ddets)
insertFastAcquisitionLoops ddets (MEDoTimes n es) = MEDoTimes n (map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (METimeLapse n dur es) = METimeLapse n dur (map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (MEStageLoop n pos es) = MEStageLoop n pos (map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (MERelativeStageLoop n ps es) = MERelativeStageLoop n ps (map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops d m = m

executeDetection :: Detector a => [a] -> [EquipmentW] -> ([Text], [DetectionParams]) -> TimeSpec -> IORef Word64 -> MVar [(AcquisitionMetaData, AcquiredData)] -> IO ()
executeDetection dets eqs (detNames, detParams) startTime dataCounter dataMVar =
    getStagePositionSafe eqs >>= \stagePos ->
    forM_ (zip detNames detParams) (\(dName, dps) ->
        setDetectorProperties dets (dpDetectors dps) >>
        let requiredDetNames = map dtpDetectorName (dpDetectors dps)
            requiredDets = filter (\d -> (detectorName d) `elem` requiredDetNames) dets
        in  mapM isConfiguredForHardwareTriggering requiredDets >>= \hasTriggering ->
            if (or hasTriggering)
            then executeFastDetectionLoop dets eqs (dName, dps) 1 startTime dataCounter dataMVar
            else switchToFilters eqs (dpFilterParams dps) >>
                 enableLightSources eqs (dpIrradiation dps) >>
                 mapConcurrently acquireData requiredDets >>= \acquiredData ->
                 disableLightSources eqs (dpIrradiation dps) >>
                 forM_ acquiredData (\acq ->
                     readIORef dataCounter >>= \idx ->
                     writeIORef dataCounter (idx + 1) >>
                     acq `deepseq` addDataToMVar dataMVar startTime idx stagePos dName acq))

setDetectorProperties :: Detector a => [a] -> [DetectorParams] -> IO ()
setDetectorProperties dets dps =
    forM_ dps (\(DetectorParams detName detOptions) ->
        let [thisDet] = filter ((==) detName . detectorName) dets
        in  mapM_ (setDetectorOption thisDet) detOptions)

executeFastDetectionLoop :: Detector a => [a] -> [EquipmentW] -> (Text, DetectionParams) -> Int -> TimeSpec -> IORef Word64 -> MVar [(AcquisitionMetaData, AcquiredData)] -> IO ()
executeFastDetectionLoop dets eqs (detName, detParams) nTimesToPerform startTime dataCounter dataMVar =
    setDetectorProperties dets (dpDetectors detParams) >>
    switchToFilters eqs (dpFilterParams detParams) >>
    fastStreamingAcquisition requiredDets enableLightSourcesAction disableLightSourcesAction detName nTimesToPerform (getStagePositionSafe eqs) startTime dataCounter dataMVar
    where
        requiredDetNames = map dtpDetectorName (dpDetectors detParams)
        requiredDets = filter (\d -> (detectorName d) `elem` requiredDetNames) dets
        nRequiredDets = length requiredDets
        enableLightSourcesAction = enableLightSources eqs (dpIrradiation detParams)
        disableLightSourcesAction = disableLightSources eqs (dpIrradiation detParams)

fastStreamingAcquisition :: Detector a => [a] -> IO () -> IO () -> Text -> Int -> IO StagePosition -> TimeSpec -> IORef Word64 -> MVar [(AcquisitionMetaData, AcquiredData)] -> IO ()
fastStreamingAcquisition requiredDets enableLightSourcesAction disableLightSourcesAction detName nTimesToPerform readStagePosFunc startTime dataCounter dataMVar =
    newChan >>= \chan ->
    readStagePosFunc >>= newIORef >>= \stagePosRef ->
    withAsync (stagePositionWorker readStagePosFunc stagePosRef) (\stageAs ->
        withAsync (acquireMultipleDetectorStreamingData requiredDets enableLightSourcesAction disableLightSourcesAction nTimesToPerform chan) (\as ->
            fetchData stagePosRef 0 0 as chan >>
            cancel stageAs >>
            wait as))
    where
        nRequiredDets = length requiredDets
        fetchData :: IORef StagePosition -> Int -> Int -> Async () -> Chan AsyncData -> IO ()
        fetchData posRef nFetched nFinished as chan
            | nFinished == nRequiredDets = wait as
            | otherwise =
                  readChan chan >>= \val ->
                  case val of
                      AsyncFinished -> performMajorGC >>
                                       fetchData posRef nFetched (nFinished + 1) as chan
                      AsyncError    -> performMajorGC >> throwIO (userError "error during fast acquisition loop")
                      AsyncData r   -> when (nFetched `mod` 50 == 49) (performMajorGC) >>
                                       readIORef posRef >>= \pos ->
                                       readIORef dataCounter >>= \idx ->
                                       writeIORef dataCounter (idx + 1) >>
                                       r `deepseq` addDataToMVar dataMVar startTime idx pos detName r >>
                                       fetchData posRef (nFetched + 1) nFinished as chan
        stagePositionWorker :: IO StagePosition -> IORef StagePosition -> IO ()
        stagePositionWorker readStagePosFunc positionRef =
            mask $ \restore ->
                forever (readStagePosFunc >>= writeIORef positionRef >>
                         restore (threadDelay 1e6))

executeIrradiation :: [EquipmentW] -> [IrradiationParams] -> LSIlluminationDuration -> IO ()
executeIrradiation eqs params dur =
    forConcurrently_ params (\(IrradiationParams eqName sourceName channels powers) ->
        let [eq] = filter (\e -> equipmentName e == eqName) eqs
        in  activateLightSourceTimed eq sourceName (zip channels powers) dur)

switchToFilters :: [EquipmentW] -> [FilterParams] -> IO ()
switchToFilters eqs fps =
    forConcurrently_ fps (\(FilterParams eqName fwName fw) -> switchFilterWheel eqs eqName fwName fw)

enableLightSources :: [EquipmentW] -> [IrradiationParams] -> IO ()
enableLightSources eqs params =
    forM_ params (\(IrradiationParams eqName sourceName channels powers) ->
        let [eq] = filter (\e -> equipmentName e == eqName) eqs
        in  activateLightSource eq sourceName (zip channels powers))

disableLightSources :: [EquipmentW] -> [IrradiationParams] -> IO ()
disableLightSources eqs params =
    forM_ params (\(IrradiationParams eqName sourceName _ _) ->
        let [eq] = filter (\e -> equipmentName e == eqName) eqs
        in  deactivateLightSource eq)

deactivateAllLightSources :: [EquipmentW] -> IO ()
deactivateAllLightSources sources = mapM_ deactivateLightSource sources

eqNamesUsedAsLightSourceIn :: DefinedDetections -> MeasurementElement -> [EqName]
eqNamesUsedAsLightSourceIn ddets me = S.toList (eqNamesUsedAsLightSourceIn' S.empty me)
    where
        eqNamesUsedAsLightSourceIn' :: Set EqName -> MeasurementElement -> Set EqName
        eqNamesUsedAsLightSourceIn' s (MEDetection dnNames) =
            let dps = map (\dn -> fromJust $ M.lookup dn ddets) dnNames
            in  s <> (S.fromList . concat $ map (map ipEquipmentName . dpIrradiation) dps)
        eqNamesUsedAsLightSourceIn' s (MEIrradiation _ ips) = s <> ((S.fromList . map ipEquipmentName) ips)
        eqNamesUsedAsLightSourceIn' s (MEDoTimes _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s (MEFastAcquisitionLoop _ (_, dp)) = s <> S.fromList (map ipEquipmentName . dpIrradiation $ dp)
        eqNamesUsedAsLightSourceIn' s (METimeLapse _ _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s (MEStageLoop _ _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s (MERelativeStageLoop _ _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s _ = s

switchFilterWheel :: [EquipmentW] -> EqName -> FWName -> FName -> IO ()
switchFilterWheel eqs eqName fwName fName =
    let [eq] = filter (\e -> equipmentName e == eqName) eqs
    in ST.timeout (floor 10e6) (switchToFilter eq fwName fName) >>= \result ->
       case result of
           Nothing -> throwIO (userError ("timeout communicating with filterwheel " ++ T.unpack (fromFWName fwName)))
           Just v -> return v

robotNamesUsedIn :: MeasurementElement -> [RobotName]
robotNamesUsedIn = foldMeasurementElement f
    where
        f :: MeasurementElement -> [RobotName]
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

addDataToMVar :: MVar [(AcquisitionMetaData, AcquiredData)] -> TimeSpec -> Word64 -> StagePosition -> Text -> AcquiredData -> IO ()
addDataToMVar dataMVar startTime idx stagePosition acqType d =
    modifyMVar_ dataMVar (\previousData ->
        when (length previousData > 250) (throwIO (userError "too many async data stored")) >>
        if (null previousData)
        then pure [(metaData, adjustTime d)]
        else pure ((metaData, adjustTime d) : previousData))
    where
       metaData = AcquisitionMetaData idx stagePosition acqType
       adjustTime :: AcquiredData -> AcquiredData
       adjustTime acqDat = let t = acqTimeStamp acqDat
                           in acqDat {acqTimeStamp = diffTimeSpec t startTime}

getStagePositionSafe :: [EquipmentW] -> IO StagePosition
getStagePositionSafe eqs =
    case (filter hasMotorizedStage eqs) of
        []    -> pure (StagePosition (-1.0) (-1.0) (-1.0) False 0)
        x : _ -> getStagePosition x
