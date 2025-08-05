{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings, NumDecimals #-}
module Measurements.MeasurementProgram (
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
import Control.Monad.IO.Class
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
import Detectors.Detector
import Equipment.Equipment
import Equipment.EquipmentTypes
import Measurements.MeasurementProgramTypes
import Measurements.MeasurementProgramVerification
import Utils.MeasurementProgramUtils
import Utils.MiscUtils
import Detectors.SCCamDetector

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

executeMeasurementElement env ddets (MERelativeStageLoop sn (RelativeStageLoopParams dx dy dz (bx, ax) (by, ay) (bz, az) returnToStarting) es) =
    withStatusMessage env "relative stage loop" (
        getStagePosition stageEq >>= \(startPosition@(StagePosition startX startY startZ usingAF afOffset)) ->
        let xCoords = map ((+) startX . (*) dx . fromIntegral) [negate bx .. ax]
            yCoords = map ((+) startY . (*) dy . fromIntegral) [negate by .. ay]
            (firstX, firstY) = (head xCoords, head yCoords)
            (lastX, lastY) = (head xCoords, head yCoords)
            extraPositionsToStart = if (not usingAF) then [] else (extraPositionsBetween (startPosition.spX, startPosition.spY) (firstX, firstY))
            extraPositionsFromEnd = if (not usingAF) then [] else (extraPositionsBetween (lastX, lastY) (startPosition.spX, startPosition.spY))
        in  forM_ extraPositionsToStart (\(x, y) ->
                setStagePosition stageEq (StagePosition x y startZ usingAF afOffset)) >>
            forM_ xCoords (\ x->
                forM_ yCoords (\y ->
                    setStagePosition stageEq (StagePosition x y startZ usingAF afOffset) >>
                    getStagePosition stageEq >>= \(StagePosition upX upY upZ upAF upOffset) ->
                    let zCoords = map ((+) upZ . (*) dz . fromIntegral) [negate bz .. az]
                    in  forM_ zCoords (\z ->
                            setStagePosition stageEq (StagePosition upX upY z upAF upOffset) >> -- predetermined pfs
                            executeMeasurementElements env ddets es
                        ))) >>
            forM_ extraPositionsFromEnd (\(x, y) ->
                setStagePosition stageEq (StagePosition x y startZ usingAF afOffset)) >>
            when (returnToStarting) (setStagePosition stageEq startPosition)
    --else
        -- if we are using an autofocus system, and the sample is uneven, the AF system may not be able to find
        -- the focus if we move too far away from the current position. So we may make intermediate stops on the way
        -- to the first stage position
        -- that makes sure we explore in the vicinity of locations with known AF positions.
    --    let
    --        (xCoords, yCoords,z_dep_list) = callGrid ax ay bx by dx dy startX startY
    --        z_list = [startZ] ++ (replicate (length(z_dep_list)-1) 0)
    --     in
    --        cyclePositions xCoords yCoords z_list z_dep_list 0 stageEq usingAF afOffset bz az dz env ddets es >>
    --        setStagePosition stageEq startPosition
    )
    where
        [stageEq] = filter (\e -> hasMotorizedStage e && motorizedStageName e == sn) (peEquipment env)
        extraPositionsBetween :: (Double, Double) -> (Double, Double) -> [(Double, Double)]
        extraPositionsBetween (xStart, yStart) (xEnd, yEnd) =
            take nAdditional $ map (\idx -> (xStart + idx * dx, yStart + idx * dy)) [1.0, 2.0.. ]
            where
                distanceX = xEnd - xStart
                distanceY = yEnd - yStart
                distance = sqrt (distanceX^2 + distanceY^2)
                nAdditional = floor (distance / maxDistanceBetweenIntermediatePoints) :: Int
                dx = distanceX / fromIntegral (nAdditional + 1)
                dy = distanceY / fromIntegral (nAdditional + 1)
                maxDistanceBetweenIntermediatePoints = 2000 -- need an extra position every this many microns when proceeding to the first point

cyclePositions xCoords yCoords zCoords z_list n stageEq usingAF afOffset bz az dz env ddtes es  =
      if n/=length(z_list)
        then
          let
              x_pos = xCoords!!n
              y_pos = yCoords!!n
              ind_n = z_list!!n
              z_pos = zCoords!!ind_n
              upd_n = n+1

          in print upd_n >> setStagePosition stageEq (StagePosition   x_pos y_pos z_pos usingAF afOffset)  >>  getStagePosition stageEq >>= \(StagePosition upX upY upZ upAF upOffset) ->
              let zVals = map ((+) upZ . (*) dz . fromIntegral) [negate bz .. az]
              in (forM_ zVals (\z ->
                      setStagePosition stageEq (StagePosition upX upY z upAF upOffset) >>  executeMeasurementElements env ddtes es )) >>  getStagePosition stageEq >>= \(StagePosition currX currY currZ upAF upOffset) ->
                      let
                          new_zlist =   getZposition  currZ zCoords n
                      in cyclePositions xCoords yCoords new_zlist z_list upd_n stageEq usingAF afOffset bz az dz env ddtes es
        else
          return ()

getZposition upZ zCoords n =
    let   (x,_:ys) = splitAt n zCoords
    in (x ++ [upZ] ++ ys)

callGrid ax ay bx by dx dy startX startY =
    let xC = map ((+) startX . (*)  dx . fromIntegral) [negate ax .. bx]
        yC = map ((+) startY . (*)  dy . fromIntegral) [negate ay .. by]
        grid =  [ [x,y] | x <- xC, y <- yC ]
        walk_x = [startX]
        walk_y = [startY]
        z_list = [0]
        visited_new = assignVisit startX startY (replicate (length(grid)) 0) grid

    in walkGrid startX startY grid visited_new walk_x walk_y z_list

walkGrid xpos ypos grid visited walk_x walk_y z_list = 
    if all (==1) visited
    then
        let x  = walk_x
            y  = walk_y
        in (x,y , z_list)
    else
        let (closest_neighbour, updated_visit, updated_walk_x , updated_walk_y, updated_z_list) = getClosestNeighbour xpos ypos grid visited walk_x walk_y z_list
        in walkGrid (closest_neighbour!!0) (closest_neighbour!!1) grid  updated_visit updated_walk_x  updated_walk_y  updated_z_list

getClosestNeighbour x_pos y_pos grid visited walked_list_x walked_list_y z_list  =
    let dist_x = [abs(x!!0 - x_pos) | x <- grid]
        dist_y = [abs(y!!1 - y_pos) | y <- grid]
        manhattan_transform = [if (( sum([dist_x!!n , dist_y!!n ])/= 0 ) && visited!!n==0 ) then sum([dist_x!!n , dist_y!!n ]) else 1e100 | n <- [0..length(dist_x)-1] ]
        closest_neighbours  = [[(grid!!n)!!0,(grid!!n)!!1] | n <- [0..length(manhattan_transform)-1],  ((manhattan_transform!!n == minimum(manhattan_transform)) && (visited!!n==0 ))]

        closest_neighbour = (getLowestCol ( getLowestRow closest_neighbours ) )!!0
        updated_visit = assignVisit (closest_neighbour!!0) (closest_neighbour!!1) visited grid
        min_val = getClosestZPos  walked_list_x  walked_list_y  (closest_neighbour!!0)  (closest_neighbour!!1)

        updated_walked_list_x = walked_list_x ++ [closest_neighbour!!0]
        updated_walked_list_y = walked_list_y ++ [closest_neighbour!!1]

        updated_z_list = z_list ++ min_val

    in (closest_neighbour, updated_visit, updated_walked_list_x, updated_walked_list_y , updated_z_list )

assignVisit x_pos y_pos visited grid =
    let ind_pos = [n  | n <- [0..length(grid)-1], (((grid!!n)!!0 == x_pos) && ((grid!!n)!!1 == y_pos) )]!!0
        (x,_:ys) = splitAt ind_pos visited
    in (x ++ [1] ++ ys)


getClosestZPos walked_list_x  walked_list_y  x_pos y_pos =
    let
        dist_x = [abs(x- x_pos) | x <-  walked_list_x]
        dist_y = [abs(y- y_pos) | y <-  walked_list_y]
        manhattan_transform = [sum([dist_x!!n , dist_y!!n ])  | n <- [0..length(dist_x)-1] ]
        minvalue = minimum( manhattan_transform)
        min_arg = fromJust (elemIndex  minvalue manhattan_transform)
    in [min_arg]

getLowestRow neighbours =
    let lowest_row = minimum([x!!0 | x <- neighbours])
    in [x | x <- neighbours, x!!0 == lowest_row]

getLowestCol neighbours =
    let lowest_col = minimum([x!!1 | x <- neighbours])
    in [x | x <- neighbours, x!!1 == lowest_col]

getXYCoords xC yC = [ [xC!!n,yC!!n] | n <- [0..length(xC)-1] ]


insertFastAcquisitionLoops :: DefinedDetections -> MeasurementElement -> MeasurementElement
insertFastAcquisitionLoops ddets (MEDoTimes n [MEDetection [dName]]) = MEFastAcquisitionLoop n (dName, fromJust $ M.lookup dName ddets)
insertFastAcquisitionLoops ddets (MEDoTimes n es) = MEDoTimes n (map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (METimeLapse n dur es) = METimeLapse n dur (map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (MEStageLoop n pos es) = MEStageLoop n pos (map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (MERelativeStageLoop n ps es) = MERelativeStageLoop n ps (map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops d m = m

executeDetection :: Detector a => [a] -> [EquipmentW] -> ([Text], [DetectionParams]) -> TimeSpec -> IORef Word64 -> MVar [AsyncMeasurementMessage] -> IO ()
executeDetection dets eqs (detNames, detParams) startTime dataCounter dataMVar =
    getStagePositionSafe eqs >>= \stagePos ->
    forM_ (zip detNames detParams) (\(dName, dps) ->
        setDetectorProperties dets (dpDetectors dps) >>
        let requiredDetNames = map dtpDetectorName (dpDetectors dps)
            requiredDets = filter (\d -> (detectorName d) `elem` requiredDetNames) dets
        in  mapM isConfiguredForHardwareTriggering requiredDets >>= \hasTriggering ->
            if (or hasTriggering)
            then executeFastDetectionLoop dets eqs (dName, dps) 1 startTime dataCounter dataMVar
            else setMovableComponents eqs (dpMovableComponents dps) >>
                 enableLightSources eqs (dpIrradiation dps) >>
                 mapConcurrently acquireData requiredDets >>= \acquiredData ->
                 disableLightSources eqs (dpIrradiation dps) >>
                 forM_ acquiredData (\acq ->
                     readIORef dataCounter >>= \idx ->
                     writeIORef dataCounter (idx + 1) >>
                     (acq `deepseq` (addDataToMVar dataMVar startTime idx stagePos dName acq))))

setDetectorProperties :: Detector a => [a] -> [DetectorParams] -> IO ()
setDetectorProperties dets dps =
    forM_ dps (\(DetectorParams detName detOptions) ->
        let [thisDet] = filter ((==) detName . detectorName) dets
        in  mapM_ (setDetectorOption thisDet) detOptions)

executeFastDetectionLoop :: Detector a => [a] -> [EquipmentW] -> (Text, DetectionParams) -> Int -> TimeSpec -> IORef Word64 -> MVar [AsyncMeasurementMessage] -> IO ()
executeFastDetectionLoop dets eqs (detName, detParams) nTimesToPerform startTime dataCounter dataMVar =
    setDetectorProperties dets (dpDetectors detParams) >>
    setMovableComponents eqs (dpMovableComponents detParams) >>
    fastStreamingAcquisition requiredDets enableLightSourcesAction disableLightSourcesAction detName nTimesToPerform (getStagePositionSafe eqs) startTime dataCounter dataMVar
    where
        requiredDetNames = map dtpDetectorName (dpDetectors detParams)
        requiredDets = filter (\d -> (detectorName d) `elem` requiredDetNames) dets
        nRequiredDets = length requiredDets
        enableLightSourcesAction = enableLightSources eqs (dpIrradiation detParams)
        disableLightSourcesAction = disableLightSources eqs (dpIrradiation detParams)

fastStreamingAcquisition :: Detector a => [a] -> IO () -> IO () -> Text -> Int -> IO StagePosition -> TimeSpec -> IORef Word64 -> MVar [AsyncMeasurementMessage] -> IO ()
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
                                       (r `deepseq` (addDataToMVar dataMVar startTime idx pos detName r)) >>
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

setMovableComponents :: [EquipmentW] -> [MovableComponentParams] -> IO ()
setMovableComponents eqs mcParams =
    forConcurrently_ mcParams (\(MovableComponentParams eqName settings) -> 
        let [eq] = filter (\e -> equipmentName e == eqName) eqs
        in  ST.timeout (floor 10e6) (moveComponent eq settings) >>= \result ->
            case result of
                Nothing -> throwIO (userError ("timeout communicating with movable component in " ++ T.unpack (fromEqName (equipmentName eq))))
                Just v -> return v)

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

addDataToMVar :: MVar [AsyncMeasurementMessage] -> TimeSpec -> Word64 -> StagePosition -> Text -> AcquiredData -> IO ()
addDataToMVar dataMVar startTime idx stagePosition acqType d =
    modifyMVar_ dataMVar (\previousData ->
        when (length previousData > 250) (throwIO (userError "too many async data stored")) >>
        if (null previousData)
        then pure [AcquiredDataMessage idx metaData (adjustTime d)]
        else pure ((AcquiredDataMessage idx metaData (adjustTime d)) : previousData))
    where
       metaData = AcquisitionMetaData stagePosition acqType
       adjustTime :: AcquiredData -> AcquiredData
       adjustTime acqDat = let t = acqTimeStamp acqDat
                           in acqDat {acqTimeStamp = diffTimeSpec t startTime}

getStagePositionSafe :: [EquipmentW] -> IO StagePosition
getStagePositionSafe eqs =
    case (filter hasMotorizedStage eqs) of
        []    -> pure (StagePosition (-1.0) (-1.0) (-1.0) False 0)
        x : _ -> getStagePosition x
