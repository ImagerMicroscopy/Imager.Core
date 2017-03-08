{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}
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
executeMeasurementElement env (MEWait dur) =
    withStatusMessage env (T.format "waiting {} s" (T.Only dur)) (
        threadDelay (round $ dur * 1e6) >> return (Right ()))
executeMeasurementElement env (MEDoTimes n es) =
    withStatusMessage env "do times" (
        forM_ (zip [1 ..] (take n . repeat $ es)) (\(index :: Int, ses) ->
            updateStatusMessage env (T.format "do times {} of {}" (index, n)) >>
            executeMeasurementElements env ses
        ) >> return (Right ()))
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
            waitUntil ts >> executeMeasurementElements env es)) >>
    return (Right ())
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
        waitUntil ts = getTime Monotonic >>= return . toNanoSecs . diffTimeSpec ts >>= \ns ->
                       threadDelay (fromIntegral (max (ns `div` 1000) 0))
executeMeasurementElement env (MEStageLoop sn poss es) =
    withStatusMessage env "stage loop" (
        forM_ (zip [1..] poss) (\(index :: Int, (posName, pos)) ->
            updateStatusMessage env (T.format "stage position {} of {} ({})" (index, nPos, posName)) >>
            setStagePosition sn pos >> executeMeasurementElements env es) >>
        return (Right ()))
    where
        stages = peMotorizedStages env
        nPos = length poss
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
