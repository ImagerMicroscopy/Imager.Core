{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings, NumDecimals #-}
module Measurements.MeasurementProgram (
    executeMeasurement
  , executeDetection
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Aeson
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
import qualified Data.Vector as V
import Data.Word
import System.Mem
import System.Clock
import qualified System.Timeout as ST

import Detectors.Detector
import Equipment.Equipment
import Equipment.EquipmentTypes
import Measurements.MeasurementProgramTypes
import Measurements.MeasurementProgramVerification
import Measurements.SmartProgram
import Camera.SCCameraTypes
import Measurements.SmartProgram
import Measurements.SmartProgramRunner
import Utils.MeasurementProgramUtils
import Utils.MiscUtils
import Utils.WaitableChannel

executeMeasurement :: Detector a => ([a], [EquipmentW], MessageChannel, MVar [Text]) -> MeasurementElement -> DefinedDetections -> SmartProgramCode -> IO ()
executeMeasurement (detectors, equipment, messageChannel, statusMVar) me ddets smartProgramCode =
    withAsync (forever $ resetSystemSleepTimer >> threadDelay (round 60.0e6)) (\_ ->
        newIORef (DetectionIndex 0) >>= \detectionIdxRef ->
        newIORef "" >>= \stagePositionNameRef ->
        TimeAtStartOfExperiment <$> getTime Monotonic >>= \startTime ->
        withSmartPrograms smartProgramCode $ \smartProgramCommunicationFuncs ->
        withWaitableChannel (sendDetectedImageToSmartPrograms_Worker (spcfSendImagesFunc smartProgramCommunicationFuncs)) $ \smartProgramsChannelWriter ->
            let smartProgramIDs = parseSmartProgramIDsFromCode smartProgramCode
                env = ProgramEnvironment detectors startTime equipment detectionIdxRef stagePositionNameRef smartProgramIDs messageChannel statusMVar smartProgramCommunicationFuncs smartProgramsChannelWriter  
            in  forM_ equipment (flushSerialPorts) >>
                forM_ (M.toList commonDetectorProperties) (\(detName, opts) ->
                    mapM_ (setDetectorOption (namedDetector detName)) opts) >>
                (executeMeasurementElement env ddetsWithoutCommon (insertFastAcquisitionLoops ddetsWithoutCommon me)
                    `catch` (\e -> deactivateUsedLightSources >>
                                mapM_ stopRobot eqWithUsedRobots >>
                                putStrLn (displayException e) >>
                                throwIO (e :: SomeException))))
    where
        namedDetector n = head (filter ((==) n . detectorName) detectors)
        eqsUsedAsLightSource = eqNamesUsedAsLightSourceIn ddets me
        deactivateUsedLightSources = mapM_ deactivateLightSource (filter (\e -> equipmentName e `elem` eqsUsedAsLightSource) equipment)
        eqWithUsedRobots = let usedRobotEqNames = equipmentNamesWithRobotsUsedInME me
                           in  filter (\e -> (equipmentName e) `elem` usedRobotEqNames) equipment
        (ddetsWithoutCommon, commonDetectorProperties) =  removeCommonDetectorProperties ddets

removeCommonDetectorProperties :: DefinedDetections -> (DefinedDetections, Map DetectorName [DetectorProperty])
removeCommonDetectorProperties ddets = (ddetsWithoutCommon, commonDetectorProperties)
    where
        allDetectorParams :: [DetectorParams]
        allDetectorParams = mconcat (map dpDetectors (M.elems ddets))
        allDetectorProperties :: Map DetectorName [[DetectorProperty]]
        allDetectorProperties = foldr (\dp accum -> M.insertWith (++) (dtpDetectorName dp) [(dtpDetectorProperties dp)] accum) M.empty allDetectorParams
        commonDetectorProperties :: Map DetectorName [DetectorProperty]
        commonDetectorProperties = M.map extractCommonDetectorProperties allDetectorProperties
            where
              extractCommonDetectorProperties :: [[DetectorProperty]] -> [DetectorProperty]
              extractCommonDetectorProperties pss =
                  let uniqueProperties = nub (mconcat pss)
                      commonProperties = filter (\prop -> all (prop `elem`) pss) uniqueProperties
                  in  commonProperties
        ddetsWithoutCommon = M.map (\detParams -> detParams {dpDetectors = removeCommon commonDetectorProperties (dpDetectors detParams)}) ddets
        removeCommon :: Map DetectorName [DetectorProperty] -> [DetectorParams] -> [DetectorParams]
        removeCommon commonmap dtorparams =
            map (\(DetectorParams name opts) -> DetectorParams name (filter (`notElem` (fromJust $ M.lookup name commonmap)) opts)) dtorparams

executeMeasurementElements :: Detector a => ProgramEnvironment a -> DefinedDetections -> [MeasurementElement] -> IO ()
executeMeasurementElements env ddets es = mapM_ (executeMeasurementElement env ddets) es

executeMeasurementElement :: Detector a => ProgramEnvironment a -> DefinedDetections -> MeasurementElement -> IO ()
executeMeasurementElement env ddets (MEDetection detNames smartProgramIDs) =
    withStatusMessage env "Detection" (
        readIORef (peDetectionIndexRef env) >>= \detIdx ->
        readIORef (peCurrentNamedPosition env) >>= \stagePositionName ->
        executeDetection detectors eqs (detNames, detParams) startTime detIdx stagePositionName messageChannel (sendToSmartProgramsChannel, smartProgramIDs)) >>
        modifyIORef (peDetectionIndexRef env) (DetectionIndex . (+1) . fromDetectionIndex)
    where
      eqs = peEquipment env
      detectors = peDetectors env
      messageChannel = peMessageChannel env
      startTime = peStartTime env
      detParams = map (\dn -> fromJust $ M.lookup dn ddets) detNames
      sendToSmartProgramsChannel = peSmartProgramSendChan env

executeMeasurementElement env _ (MEIrradiation dur ips) =
    withStatusMessage env (T.format "irradiating {} s" (T.Only (fromLSIlluminationDuration dur))) (
        executeIrradiation eqs ips dur)
    where
      eqs = peEquipment env

executeMeasurementElement env _ (MEWait (WaitDuration dur)) =
    withStatusMessage env (T.format "waiting {} s" (T.Only dur)) (
        threadDelay (round $ dur * 1e6))

executeMeasurementElement env _ (MEExecuteRobotProgram (RobotProgramExecutionParams eqName robotName progName progArgs)) =
    withStatusMessage env (T.format "executing program {} on {}/{}" (fromRobotProgramName progName, fromRobotName robotName,  fromEqName eqName)) (
        executeRobotProgram eq robotName progName progArgs)
    where
        [eq] = filter ((== eqName) . equipmentName) (peEquipment env)

executeMeasurementElement env ddets (MEDoTimes n maybeDecisionFromSmartProgramID es) =
    maybeUpdateLoopCount maybeDecisionFromSmartProgramID n >>= \n' ->
    withStatusMessage env "do times" (
        forM_ (zip [1 ..] (take (fromNumIterationsTotal n') . repeat $ es)) (\(index :: Int, ses) ->
            updateStatusMessage env (T.format "do times {} of {}" (index, (fromNumIterationsTotal n'))) >>
            executeMeasurementElements env ddets ses
        ))
    where
        messageChannel = peMessageChannel env
        timeAtExpStart = peStartTime env
        getDoTimesDecisionFunc = spcfGetSmartProgramDoTimesDecisionFunc (peSmartProgramCommunicationFuncs env)
        maybeUpdateLoopCount :: Maybe SmartProgramID -> NumIterationsTotal -> IO NumIterationsTotal
        maybeUpdateLoopCount maybeID n 
                | isNothing maybeID = pure n
                | otherwise = waitUntilWaitableChannelIsEmpty (peSmartProgramSendChan env) >>
                              getDoTimesDecisionFunc (fromJust maybeID) >>= \decision ->
                              TimeAtStartOfEvent <$> getTime Monotonic >>= \timeAtDecision ->
                              addDataToChannel messageChannel (smartProgramDecisionAsMessage decision (fromJust maybeID) timeAtExpStart timeAtDecision) >>
                                  case decision of
                                      ResponseNoDecision _         -> pure n
                                      ResponseDoTimesDecision n' _ -> pure n'

executeMeasurementElement env ddets (MEFastAcquisitionLoop n (detName, detParams) maybeDecisionFromSmartProgramID programIDs) =
    maybeUpdateLoopCount maybeDecisionFromSmartProgramID n >>= \n' ->
    withStatusMessage env (T.format "fast acquisition ({} images)" (T.Only (fromNumIterationsTotal n'))) (
        readIORef (peDetectionIndexRef env) >>= \detectionIndex ->
        readIORef (peCurrentNamedPosition env) >>= \stagePositionName ->
        executeFastDetectionLoop detectors eqs (detName, detParams) n' startTime detectionIndex stagePositionName messageChannel (sendToSmartProgramsChannel, programIDs) >>
        modifyIORef (peDetectionIndexRef env) (DetectionIndex . ((+) (fromNumIterationsTotal n')) . fromDetectionIndex))
    where
        eqs = peEquipment env
        messageChannel = peMessageChannel env
        startTime = peStartTime env
        detectors = peDetectors env
        sendToSmartProgramsChannel = peSmartProgramSendChan env
        getDoTimesDecisionFunc = spcfGetSmartProgramDoTimesDecisionFunc (peSmartProgramCommunicationFuncs env)
        maybeUpdateLoopCount :: Maybe SmartProgramID -> NumIterationsTotal -> IO NumIterationsTotal
        maybeUpdateLoopCount maybeID n 
                | isNothing maybeID = pure n
                | otherwise = waitUntilWaitableChannelIsEmpty (peSmartProgramSendChan env) >>
                              getDoTimesDecisionFunc (fromJust maybeID) >>= \decision ->
                              TimeAtStartOfEvent <$> getTime Monotonic >>= \timeAtDecision ->
                              addDataToChannel messageChannel (smartProgramDecisionAsMessage decision (fromJust maybeID) startTime timeAtDecision) >>
                                  case decision of
                                      ResponseNoDecision _         -> pure n
                                      ResponseDoTimesDecision n' _ -> pure n'

executeMeasurementElement env ddets (METimeLapse n dur maybeDecisionFromSmartProgramID es) =
    withStatusMessage env "time lapse" (
        maybeUpdateLoopParameters maybeDecisionFromSmartProgramID dur n >>= \(n', dur') ->
        futureTimes dur' n' >>= \fts ->
        timeSpecToUTCTimes fts >>= \utcs ->
        forM_ (zip3 [1 ..] fts utcs) (\(index :: Int, ts, utc) ->
            formattedTime utc >>= \timeStr ->
            updateStatusMessage env (T.format "next time lapse ({} of {}) at {}" (index, (fromNumIterationsTotal n'), timeStr)) >>
            waitUntil ts >>
            updateStatusMessage env (T.format "executing time lapse {} of {}" (index, (fromNumIterationsTotal n'))) >>
            executeMeasurementElements env ddets es))
    where
        messageChannel = peMessageChannel env
        timeAtExpStart = peStartTime env
        getTimeLapseDecisionFunc = spcfGetSmartProgramTimeLapseDecisionFunc (peSmartProgramCommunicationFuncs env)
        maybeUpdateLoopParameters :: Maybe SmartProgramID -> WaitDuration -> NumIterationsTotal -> IO (NumIterationsTotal, WaitDuration)
        maybeUpdateLoopParameters maybeInputProgramID dur n
            | isNothing maybeInputProgramID = pure (n, dur)
            | otherwise = waitUntilWaitableChannelIsEmpty (peSmartProgramSendChan env) >>
                          getTimeLapseDecisionFunc (fromJust maybeInputProgramID) >>= \decision ->
                          TimeAtStartOfEvent <$> getTime Monotonic >>= \timeAtDecision ->
                          addDataToChannel messageChannel (smartProgramDecisionAsMessage decision (fromJust maybeInputProgramID) timeAtExpStart timeAtDecision) >>
                              case decision of
                                  ResponseNoDecision _                -> pure (n, dur)
                                  ResponseTimeLapseDecision n' dur' _ -> pure (n', dur')
        futureDurations dur n = map ((*) (fromWaitDuration dur) . fromIntegral) [0 .. ((fromNumIterationsTotal n) - 1)]
        futureTimes :: WaitDuration -> NumIterationsTotal -> IO [TimeSpec]
        futureTimes dur n = getTime Monotonic >>= \now ->
                            return $ map (\delta -> fromNanoSecs (toNanoSecs now + round (delta * 1.0e9))) (futureDurations dur n)
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

executeMeasurementElement env ddets (MEStageLoop sn poss maybeDecisionFromSmartProgramID es) =
    withStatusMessage env "stage loop" (
        readIORef (peCurrentNamedPosition env) >>= \savedPositionName -> -- restore previous name in case of nested stage loops
        maybeUpdateStagePositions maybeDecisionFromSmartProgramID poss >>= \poss' ->
        forM_ (zip [1..] poss') (\(index :: Int, (PositionNameAndCoords posName pos)) ->
            updateStatusMessage env (T.format "stage position {} of {} ({})" (index, (length poss'), posName)) >>
            writeIORef (peCurrentNamedPosition env) posName >>
            setStagePosition stageEq pos >> executeMeasurementElements env ddets es) >>
        writeIORef (peCurrentNamedPosition env) savedPositionName)
    where
        [stageEq] = filter (\e -> hasMotorizedStage e && motorizedStageName e == sn) (peEquipment env)
        messageChannel = peMessageChannel env
        timeAtExpStart = peStartTime env
        getStageLoopDecisionFunc = spcfGetSmartProgramStageLoopDecisionFunc (peSmartProgramCommunicationFuncs env)
        maybeUpdateStagePositions :: Maybe SmartProgramID -> [PositionNameAndCoords] -> IO [PositionNameAndCoords]
        maybeUpdateStagePositions maybeID poss 
            | isNothing maybeID = pure poss
            | otherwise = waitUntilWaitableChannelIsEmpty (peSmartProgramSendChan env) >>
                          getStageLoopDecisionFunc (fromJust maybeID) >>= \decision ->
                          TimeAtStartOfEvent <$> getTime Monotonic >>= \timeAtDecision ->
                          addDataToChannel messageChannel (smartProgramDecisionAsMessage decision (fromJust maybeID) timeAtExpStart timeAtDecision) >>
                                  case decision of
                                      ResponseNoDecision _              -> pure poss
                                      ResponseStageLoopDecision poss' _ -> pure poss'

executeMeasurementElement env ddets (MERelativeStageLoop sn params maybeDecisionFromSmartProgramID es) =
    withStatusMessage env "relative stage loop" (
        maybeUpdateParameters maybeDecisionFromSmartProgramID params >>= \params' ->
        getStagePosition stageEq >>= \(startPosition@(StagePosition startX startY startZ usingAF afOffset)) ->
        let (RelativeStageLoopParams dx dy dz (bx, ax) (by, ay) (bz, az) returnToStarting) = params'
            xCoords = map ((+) startX . (*) dx . fromIntegral) [negate bx .. ax]
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
    )
    where
        [stageEq] = filter (\e -> hasMotorizedStage e && motorizedStageName e == sn) (peEquipment env)
        messageChannel = peMessageChannel env
        timeAtExpStart = peStartTime env
        getRelativeStageLoopDecisionFunc = spcfGetSmartProgramRelativeStageLoopDecisionFunc (peSmartProgramCommunicationFuncs env)
        maybeUpdateParameters :: Maybe SmartProgramID -> RelativeStageLoopParams -> IO RelativeStageLoopParams
        maybeUpdateParameters maybeID params 
            | isNothing maybeID = pure params
            | otherwise = waitUntilWaitableChannelIsEmpty (peSmartProgramSendChan env) >>
                          getRelativeStageLoopDecisionFunc (fromJust maybeID) >>= \decision ->
                          TimeAtStartOfEvent <$> getTime Monotonic >>= \timeAtDecision ->
                          addDataToChannel messageChannel (smartProgramDecisionAsMessage decision (fromJust maybeID) timeAtExpStart timeAtDecision) >>
                              case decision of
                                  ResponseNoDecision _                        -> pure params
                                  ResponseRelativeStageLoopDecision params' _ -> pure params'
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
insertFastAcquisitionLoops ddets (MEDoTimes n inputSmartID [MEDetection [dName] smartIDs]) = MEFastAcquisitionLoop n (dName, fromJust $ M.lookup dName ddets) inputSmartID smartIDs
insertFastAcquisitionLoops ddets (MEDoTimes n ids es) = MEDoTimes n ids (map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (METimeLapse n dur ids es) = METimeLapse n dur ids(map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (MEStageLoop n pos ids es) = MEStageLoop n pos ids(map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (MERelativeStageLoop n ps ids es) = MERelativeStageLoop n ps ids(map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops d m = m

executeDetection :: Detector a => [a] -> [EquipmentW] -> ([AcquisitionTypeName], [DetectionParams]) -> TimeAtStartOfExperiment -> 
                                  DetectionIndex -> Text -> MessageChannel -> (SendToSmartProgramsChannelWriter, [SmartProgramID]) -> IO ()
executeDetection dets eqs (acqTypeNames, detParams) expStartTime detectionIndex stagePositionName messageChannel (smartProgramsChannel, smartProgramIDs) =
    getStagePositionSafe eqs >>= \stagePos ->
    forM_ (zip acqTypeNames detParams) (\(acqTypeName, dps) ->
        setDetectorProperties dets (dpDetectors dps) >>
        let requiredDetNames = map dtpDetectorName (dpDetectors dps)
            requiredDets = filter (\d -> (detectorName d) `elem` requiredDetNames) dets
        in  mapM isConfiguredForHardwareTriggering requiredDets >>= \hasTriggering ->
            if (or hasTriggering)
            then executeFastDetectionLoop dets eqs (acqTypeName, dps) (NumIterationsTotal 1) expStartTime detectionIndex stagePositionName messageChannel (smartProgramsChannel, smartProgramIDs)
            else setMovableComponents eqs (dpMovableComponents dps) >>
                 enableLightSources eqs (dpIrradiation dps) >>
                 TimeAtStartOfEvent <$> getTime Monotonic >>= \detStartTime ->
                 mapConcurrently acquireData requiredDets >>= \measuredImages ->
                 disableLightSources eqs (dpIrradiation dps) >>
                 forM_ (zip measuredImages (map detectorName requiredDets)) (\(measuredImage, detName) ->
                     let acquiredData = measuredImageAsAcquiredData measuredImage detName expStartTime detStartTime
                         metadata = AcquisitionMetaData stagePos stagePositionName acqTypeName detectionIndex nImagesMeasuredInThisDetection
                     in  (acquiredData `deepseq` (addDataToChannel messageChannel (AcquiredDataMessage metadata acquiredData))) >>
                         when (not $ null smartProgramIDs) (
                            writeWriteableChannel smartProgramsChannel (smartProgramIDs, (metadata, acquiredData)))))
    where
        nImagesMeasuredInThisDetection :: NumImagesInDetection
        nImagesMeasuredInThisDetection = NumImagesInDetection $ foldl' (\idx detParam -> idx + length (dpDetectors detParam)) 0 detParams

setDetectorProperties :: Detector a => [a] -> [DetectorParams] -> IO ()
setDetectorProperties dets dps =
    forM_ dps (\(DetectorParams detName detOptions) ->
        let [thisDet] = filter ((==) detName . detectorName) dets
        in  mapM_ (setDetectorOption thisDet) detOptions)

executeFastDetectionLoop :: Detector a => [a] -> [EquipmentW] -> (AcquisitionTypeName, DetectionParams) -> NumIterationsTotal -> 
                                                 TimeAtStartOfExperiment -> DetectionIndex -> Text -> MessageChannel -> (SendToSmartProgramsChannelWriter, [SmartProgramID]) -> IO ()
executeFastDetectionLoop dets eqs (detName, detParams) nTimesToPerform expStartTime detectionIndex stagePositionName messageChannel smartProgsIDs =
    setDetectorProperties dets (dpDetectors detParams) >>
    setMovableComponents eqs (dpMovableComponents detParams) >>
    fastStreamingAcquisition requiredDets enableLightSourcesAction disableLightSourcesAction detName nTimesToPerform (getStagePositionSafe eqs) 
                             expStartTime detectionIndex stagePositionName messageChannel smartProgsIDs
    where
        requiredDetNames = map dtpDetectorName (dpDetectors detParams)
        requiredDets = filter (\d -> (detectorName d) `elem` requiredDetNames) dets
        nRequiredDets = length requiredDets
        enableLightSourcesAction = enableLightSources eqs (dpIrradiation detParams)
        disableLightSourcesAction = disableLightSources eqs (dpIrradiation detParams)

fastStreamingAcquisition :: Detector a => [a] -> IO () -> IO () -> AcquisitionTypeName -> NumIterationsTotal -> IO StagePosition -> 
                                          TimeAtStartOfExperiment -> DetectionIndex -> Text -> MessageChannel -> (SendToSmartProgramsChannelWriter, [SmartProgramID]) -> IO ()
fastStreamingAcquisition requiredDets enableLightSourcesAction disableLightSourcesAction acqName (NumIterationsTotal nTimesToPerform)
                         readStagePosFunc expStartTime (DetectionIndex detectionIndex) stagePositionName messageChannel (smartProgramsChannel, smartProgramIDs) =
    newChan >>= \chan ->
    readStagePosFunc >>= newIORef >>= \stagePosRef ->
    withAsync (stagePositionWorker readStagePosFunc stagePosRef) (\stageAs ->
        TimeAtStartOfEvent <$> getTime Monotonic >>= \detStartTime ->
        withAsync (acquireMultipleDetectorStreamingData requiredDets enableLightSourcesAction disableLightSourcesAction nTimesToPerform chan) (\async ->
            fetchData detStartTime stagePosRef numImagesAcquiredMap 0 async chan >>
            cancel stageAs >>
            wait async))
    where
        nRequiredDets = length requiredDets
        numImagesPerDetectionIndex = NumImagesInDetection nRequiredDets
        numImagesAcquiredMap :: Map DetectorName Int
        numImagesAcquiredMap = M.fromList (zip (map detectorName requiredDets) (repeat 0))  -- keeps track of how many images have been acquired for each detector
        fetchData :: TimeAtStartOfEvent -> IORef StagePosition -> Map DetectorName Int -> Int -> Async () -> Chan AsyncData -> IO ()
        fetchData detStartTime posRef numImagesAcquiredMap nDetectorsFinished async chan
            | nDetectorsFinished == nRequiredDets = wait async
            | otherwise =
                  readChan chan >>= \val ->
                  case val of
                      AsyncFinished -> performMajorGC >>
                                       fetchData detStartTime posRef numImagesAcquiredMap (nDetectorsFinished + 1) async chan
                      AsyncError    -> performMajorGC >> throwIO (userError "error during fast acquisition loop")
                      AsyncData (detName, measuredImage) -> 
                                       readIORef posRef >>= \pos ->
                                       let thisDetIdx = detectionIndex + fromJust (M.lookup detName numImagesAcquiredMap)
                                           nImagesAcquiredTotal = sum (M.elems numImagesAcquiredMap)
                                           newMap = M.adjust (+1) detName numImagesAcquiredMap
                                           acquiredData = measuredImageAsAcquiredData measuredImage detName expStartTime detStartTime
                                           metadata = AcquisitionMetaData pos stagePositionName acqName (DetectionIndex thisDetIdx) numImagesPerDetectionIndex
                                       in  when (nImagesAcquiredTotal `mod` 50 == 49) (performMajorGC) >>
                                           (acquiredData `deepseq` (addDataToChannel messageChannel (AcquiredDataMessage metadata acquiredData))) >>
                                           when (not . null $ smartProgramIDs) (
                                               writeWriteableChannel smartProgramsChannel (smartProgramIDs, (metadata, acquiredData))) >>
                                           fetchData detStartTime posRef newMap nDetectorsFinished async chan
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
        eqNamesUsedAsLightSourceIn' s (MEDetection dnNames _) =
            let dps = map (\dn -> fromJust $ M.lookup dn ddets) dnNames
            in  s <> (S.fromList . concat $ map (map ipEquipmentName . dpIrradiation) dps)
        eqNamesUsedAsLightSourceIn' s (MEIrradiation _ ips) = s <> ((S.fromList . map ipEquipmentName) ips)
        eqNamesUsedAsLightSourceIn' s (MEDoTimes _ _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s (MEFastAcquisitionLoop _ (_, dp) _ _) = s <> S.fromList (map ipEquipmentName . dpIrradiation $ dp)
        eqNamesUsedAsLightSourceIn' s (METimeLapse _ _ _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s (MEStageLoop _ _ _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s (MERelativeStageLoop _ _ _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s _ = s

equipmentNamesWithRobotsUsedInME :: MeasurementElement -> [EqName]
equipmentNamesWithRobotsUsedInME = foldMeasurementElement f
    where
        f :: MeasurementElement -> [EqName]
        f (MEExecuteRobotProgram (RobotProgramExecutionParams eqName _ _ _)) = [eqName]
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

addDataToChannel :: MessageChannel -> AsyncMeasurementMessage -> IO ()
addDataToChannel messageChannel msg =
    numMessagesInChannel messageChannel >>= \nMessages ->
    when (nMessages > 250) (throwIO (userError "too many async data stored")) >>
    sendMessageToChannel messageChannel msg

getStagePositionSafe :: [EquipmentW] -> IO StagePosition
getStagePositionSafe eqs =
    case (filter hasMotorizedStage eqs) of
        []    -> pure (StagePosition (-1.0) (-1.0) (-1.0) False 0)
        x : _ -> getStagePosition x

withSmartPrograms :: SmartProgramCode -> (SmartProgramCommunicationFunctions -> IO ()) -> IO ()
withSmartPrograms cs@(DAGOrchestratorCode{}) action = withSmartProgramServer cs action
withSmartPrograms (ProgramRunnerCode cs) action = withRunnableSmartPrograms cs action
