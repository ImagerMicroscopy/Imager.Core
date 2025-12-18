{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings, NumDecimals #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
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
import Measurements.SmartProgram (getUpdatedAcquisitionDecision)
import Camera.SCCameraTypes
import Measurements.SmartProgram
import Measurements.SmartProgramRunner
import Utils.MeasurementProgramUtils
import Utils.MiscUtils
import Utils.WaitableChannel
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LB

executeMeasurement :: Detector a => ([a], [EquipmentW], MessageChannel, MVar [Text]) -> MeasurementElement -> DefinedDetections -> SmartProgramCode -> IO ()
executeMeasurement (detectors, equipment, messageChannel, statusMVar) me defineddets smartProgramCode = do
    ddets <- newIORef defineddets
    ddetsWithoutCommonIO <- newIORef ddetsWithoutCommon 

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
                (executeMeasurementElement env ddets ddetsWithoutCommonIO (insertFastAcquisitionLoops ddetsWithoutCommon me)
                    `catch` (\e -> deactivateUsedLightSources >>
                                mapM_ stopRobot eqWithUsedRobots >>
                                putStrLn (displayException e) >>
                                throwIO (e :: SomeException))))
    where
        namedDetector n = head (filter ((==) n . detectorName) detectors)
        eqsUsedAsLightSource = eqNamesUsedAsLightSourceIn defineddets me
        deactivateUsedLightSources = mapM_ deactivateLightSource (filter (\e -> equipmentName e `elem` eqsUsedAsLightSource) equipment)
        eqWithUsedRobots = let usedRobotEqNames = equipmentNamesWithRobotsUsedInME me
                           in  filter (\e -> (equipmentName e) `elem` usedRobotEqNames) equipment
        (ddetsWithoutCommon, commonDetectorProperties) =  removeCommonDetectorProperties defineddets

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

executeMeasurementElements :: Detector a => ProgramEnvironment a -> IORef DefinedDetections -> IORef DefinedDetections -> [MeasurementElement] -> IO ()
executeMeasurementElements env fullddets ddets es = mapM_ (executeMeasurementElement env fullddets ddets) es

executeMeasurementElement :: Detector a => ProgramEnvironment a ->IORef DefinedDetections -> IORef DefinedDetections -> MeasurementElement -> IO ()
executeMeasurementElement env _ defineddets (MEDetection elementID detNames smartProgramIDs) =
    withStatusMessage env "Detection" (
        readIORef (peDetectionIndexRef env) >>= \detIdx ->
        readIORef defineddets >>= \ddets -> 
        readIORef (peCurrentNamedPosition env) >>= \stagePositionName ->
        executeDetection detectors eqs (detNames, map (\dn -> fromJust $ M.lookup dn ddets) detNames) startTime detIdx stagePositionName elementID messageChannel (sendToSmartProgramsChannel, smartProgramIDs)) >>
        modifyIORef (peDetectionIndexRef env) (DetectionIndex . (+1) . fromDetectionIndex)
    where
      eqs = peEquipment env
      detectors = peDetectors env
      messageChannel = peMessageChannel env
      startTime = peStartTime env

      sendToSmartProgramsChannel = peSmartProgramSendChan env

executeMeasurementElement env fullddets ddets (MEUpdateAcquisition elementID programID acquisitionName) =
    withStatusMessage env "Updating Acquisition" $ do
        toChangeDetsMap <- readIORef fullddets
        waitUntilWaitableChannelIsEmpty (peSmartProgramSendChan env) 
        maybeUpdated <- getUpdatedAcquisitionDecision (fromJust programID)  elementID  toChangeDetsMap (fromJust acquisitionName)

        let definedDetsMap = fromMaybe toChangeDetsMap maybeUpdated
        writeIORef fullddets definedDetsMap

        case maybeUpdated of
            Just updated -> do
                let (ddetsWithoutCommon, commonDetectorProperties) =
                        removeCommonDetectorProperties updated
                writeIORef ddets ddetsWithoutCommon

                let detectors = peDetectors env  
                    namedDetector n = head (filter ((== n) . detectorName) detectors)

                forM_ (M.toList commonDetectorProperties) $ \(detName, opts) ->
                    forM_ opts $ \opt ->
                        setDetectorOption (namedDetector detName) opt

            Nothing ->
                putStrLn "No update to acquisition decision was available."



executeMeasurementElement env _ _ (MEIrradiation _ dur ips) =
    withStatusMessage env (T.format "irradiating {} s" (T.Only (fromLSIlluminationDuration dur))) (
        executeIrradiation eqs ips dur)
    where
      eqs = peEquipment env

executeMeasurementElement env _ _ (MEWait _ (WaitDuration dur)) =
    withStatusMessage env (T.format "waiting {} s" (T.Only dur)) (
        threadDelay (round $ dur * 1e6))

executeMeasurementElement env _ _ (MEExecuteRobotProgram _ (RobotProgramExecutionParams eqName robotName progName progArgs)) =
    withStatusMessage env (T.format "executing program {} on {}/{}" (fromRobotProgramName progName, fromRobotName robotName,  fromEqName eqName)) (
        executeRobotProgram eq robotName progName progArgs)
    where
        [eq] = filter ((== eqName) . equipmentName) (peEquipment env)

executeMeasurementElement env fullddets ddets (MEDoTimes _ n maybeDecisionFromSmartProgramID es) =
    maybeUpdateLoopCount maybeDecisionFromSmartProgramID n >>= \n' ->
    withStatusMessage env "do times" (
        forM_ (zip [1 ..] (take (fromNumIterationsTotal n') . repeat $ es)) (\(index :: Int, ses) ->
            updateStatusMessage env (T.format "do times {} of {}" (index, (fromNumIterationsTotal n'))) >>
            executeMeasurementElements env fullddets ddets ses
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

executeMeasurementElement env fullddets ddets (MEFastAcquisitionLoop detectionID n (detName, detParams) maybeDecisionFromSmartProgramID programIDs) =
    maybeUpdateLoopCount maybeDecisionFromSmartProgramID n >>= \n' ->
    withStatusMessage env (T.format "fast acquisition ({} images)" (T.Only (fromNumIterationsTotal n'))) (
        readIORef (peDetectionIndexRef env) >>= \detectionIndex ->
        readIORef (peCurrentNamedPosition env) >>= \stagePositionName ->
        executeFastDetectionLoop detectors eqs (detName, detParams) n' startTime detectionIndex stagePositionName detectionID messageChannel (sendToSmartProgramsChannel, programIDs) >>
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

executeMeasurementElement env fullddets ddets (METimeLapse _ n dur maybeDecisionFromSmartProgramID es) =
    withStatusMessage env "time lapse" (
        maybeUpdateLoopParameters maybeDecisionFromSmartProgramID dur n >>= \(n', dur') ->
        futureTimes dur' n' >>= \fts ->
        timeSpecToUTCTimes fts >>= \utcs ->
        forM_ (zip3 [1 ..] fts utcs) (\(index :: Int, ts, utc) ->
            formattedTime utc >>= \timeStr ->
            updateStatusMessage env (T.format "next time lapse ({} of {}) at {}" (index, (fromNumIterationsTotal n'), timeStr)) >>
            waitUntil ts >>
            updateStatusMessage env (T.format "executing time lapse {} of {}" (index, (fromNumIterationsTotal n'))) >>
            executeMeasurementElements env fullddets ddets es))
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

executeMeasurementElement env fullddets ddets (MEStageLoop _ sn poss maybeDecisionFromSmartProgramID es) =
    withStatusMessage env "stage loop" (
        readIORef (peCurrentNamedPosition env) >>= \savedPositionName -> -- restore previous name in case of nested stage loops
        maybeUpdateStagePositions maybeDecisionFromSmartProgramID poss >>= \poss' ->
        forM_ (zip [1..] poss') (\(index :: Int, (PositionNameAndCoords posName pos)) ->
            updateStatusMessage env (T.format "stage position {} of {} ({})" (index, (length poss'), posName)) >>
            writeIORef (peCurrentNamedPosition env) posName >>
            setStagePosition stageEq pos >> executeMeasurementElements env fullddets ddets es) >>
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

executeMeasurementElement env fullddets ddets (MERelativeStageLoop _ sn params maybeDecisionFromSmartProgramID es) =
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
                            executeMeasurementElements env fullddets ddets es
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




insertFastAcquisitionLoops :: DefinedDetections -> MeasurementElement -> MeasurementElement
insertFastAcquisitionLoops ddets (MEDoTimes _ n inputSmartID [MEDetection detectionID [dName] smartIDs]) = MEFastAcquisitionLoop detectionID n (dName, fromJust $ M.lookup dName ddets) inputSmartID smartIDs
insertFastAcquisitionLoops ddets (MEDoTimes eid n ids es) = MEDoTimes eid n ids (map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (METimeLapse eid n dur ids es) = METimeLapse eid n dur ids (map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (MEStageLoop eid n pos ids es) = MEStageLoop eid n pos ids(map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops ddets (MERelativeStageLoop eid n ps ids es) = MERelativeStageLoop eid n ps ids(map (insertFastAcquisitionLoops ddets) es)
insertFastAcquisitionLoops d m = m

executeDetection :: Detector a => [a] -> [EquipmentW] -> ([AcquisitionTypeName], [DetectionParams]) -> TimeAtStartOfExperiment -> 
                                  DetectionIndex -> Text -> ElementID -> MessageChannel -> (SendToSmartProgramsChannelWriter, [SmartProgramID]) -> IO ()
executeDetection dets eqs (acqTypeNames, detParams) expStartTime detectionIndex stagePositionName detectionElementID
                 messageChannel (smartProgramsChannel, smartProgramIDs) = do

    stagePos <- getStagePositionSafe eqs

    forM_ (zip acqTypeNames detParams) $ \(acqTypeName, dps) -> do
        setDetectorProperties dets (dpDetectors dps)
        let requiredDetNames = map dtpDetectorName (dpDetectors dps)
            requiredDets = filter (\d -> detectorName d `elem` requiredDetNames) dets
        hasTriggering <- mapM isConfiguredForHardwareTriggering requiredDets
        if or hasTriggering
            then executeFastDetectionLoop dets eqs (acqTypeName, dps) (NumIterationsTotal 1)
                                          expStartTime detectionIndex stagePositionName detectionElementID
                                          messageChannel (smartProgramsChannel, smartProgramIDs)
            else do
                setMovableComponents eqs (dpMovableComponents dps)
                enableLightSources eqs (dpIrradiation dps)
                detStartTime <- TimeAtStartOfEvent <$> getTime Monotonic
                measuredImages <- mapConcurrently acquireData requiredDets
                disableLightSources eqs (dpIrradiation dps)
                forM_ (zip measuredImages (map detectorName requiredDets)) $ \(measuredImage, detName) -> do
                    let acquiredData = measuredImageAsAcquiredData measuredImage detName expStartTime detStartTime
                        metadata = AcquisitionMetaData stagePos stagePositionName acqTypeName detectionIndex
                                                           nImagesMeasuredInThisDetection detectionElementID
                    acquiredData `deepseq` addDataToChannel messageChannel (AcquiredDataMessage metadata acquiredData)
                    when (not $ null smartProgramIDs) $
                        writeWriteableChannel smartProgramsChannel (smartProgramIDs, (metadata, acquiredData))
  where
    nImagesMeasuredInThisDetection :: NumImagesInDetection
    nImagesMeasuredInThisDetection = NumImagesInDetection $
        foldl' (\idx detParam -> idx + length (dpDetectors detParam)) 0 detParams


setDetectorProperties :: Detector a => [a] -> [DetectorParams] -> IO ()
setDetectorProperties dets dps =
    forM_ dps (\(DetectorParams detName detOptions) ->
        let [thisDet] = filter ((==) detName . detectorName) dets
        in  mapM_ (setDetectorOption thisDet) detOptions)

executeFastDetectionLoop :: Detector a => [a] -> [EquipmentW] -> (AcquisitionTypeName, DetectionParams) -> NumIterationsTotal -> 
                                                 TimeAtStartOfExperiment -> DetectionIndex -> Text -> ElementID -> MessageChannel -> (SendToSmartProgramsChannelWriter, [SmartProgramID]) -> IO ()
executeFastDetectionLoop dets eqs (detName, detParams) nTimesToPerform expStartTime detectionIndex stagePositionName 
                         detectionElementID messageChannel smartProgsIDs =
    setDetectorProperties dets (dpDetectors detParams) >>
    setMovableComponents eqs (dpMovableComponents detParams) >>
    fastStreamingAcquisition requiredDets enableLightSourcesAction disableLightSourcesAction detName nTimesToPerform (getStagePositionSafe eqs) 
                             expStartTime detectionIndex stagePositionName detectionElementID messageChannel smartProgsIDs
    where
        requiredDetNames = map dtpDetectorName (dpDetectors detParams)
        requiredDets = filter (\d -> (detectorName d) `elem` requiredDetNames) dets
        nRequiredDets = length requiredDets
        enableLightSourcesAction = enableLightSources eqs (dpIrradiation detParams)
        disableLightSourcesAction = disableLightSources eqs (dpIrradiation detParams)

fastStreamingAcquisition :: Detector a => [a] -> IO () -> IO () -> AcquisitionTypeName -> NumIterationsTotal -> IO StagePosition -> 
                                          TimeAtStartOfExperiment -> DetectionIndex -> Text -> ElementID -> MessageChannel -> (SendToSmartProgramsChannelWriter, [SmartProgramID]) -> IO ()
fastStreamingAcquisition requiredDets enableLightSourcesAction disableLightSourcesAction acqName (NumIterationsTotal nTimesToPerform)
                         readStagePosFunc expStartTime (DetectionIndex detectionIndex) stagePositionName detectionElementID
                         messageChannel (smartProgramsChannel, smartProgramIDs) =
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
                                           metadata = AcquisitionMetaData pos stagePositionName acqName (DetectionIndex thisDetIdx) numImagesPerDetectionIndex detectionElementID
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
        eqNamesUsedAsLightSourceIn' s (MEDetection _ dnNames _) =
            let dps = map (\dn -> fromJust $ M.lookup dn ddets) dnNames
            in  s <> (S.fromList . concat $ map (map ipEquipmentName . dpIrradiation) dps)
        eqNamesUsedAsLightSourceIn' s (MEIrradiation _ _ ips) = s <> ((S.fromList . map ipEquipmentName) ips)
        eqNamesUsedAsLightSourceIn' s (MEDoTimes _ _ _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s (MEFastAcquisitionLoop _ _ (_, dp) _ _) = s <> S.fromList (map ipEquipmentName . dpIrradiation $ dp)
        eqNamesUsedAsLightSourceIn' s (METimeLapse _ _ _ _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s (MEStageLoop _ _ _ _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s (MERelativeStageLoop _ _ _ _ mes) = s <> mconcat (map (eqNamesUsedAsLightSourceIn' S.empty) mes)
        eqNamesUsedAsLightSourceIn' s _ = s

equipmentNamesWithRobotsUsedInME :: MeasurementElement -> [EqName]
equipmentNamesWithRobotsUsedInME = foldMeasurementElement f
    where
        f :: MeasurementElement -> [EqName]
        f (MEExecuteRobotProgram _ (RobotProgramExecutionParams eqName _ _ _)) = [eqName]
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
