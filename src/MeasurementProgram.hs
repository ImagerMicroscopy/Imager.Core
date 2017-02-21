module MeasurementProgram where

import Control.Monad
import Data.Text (Text)
import System.Clock

import MeasurementProgramTypes
import MiscUtils

executeMeasurementElements :: ProgramEnvironment -> [MeasurementElement] -> IO (Either String ())
executeMeasurementElements env es = (sequenceExcept . map (executeMeasurementElement env)) es

executeMeasurementElement :: ProgramEnvironment -> MeasurementElement -> IO (Either String ())
executeMeasurementElement env (MEDetection detParams) =
    forM_ detParams $ \p ->
        mapM (executeDetection lss fws) p >>= return . map fromRight >>=
            addDataToMVar mvar startTime
    where
      lss = peLightSources env
      fws = peFilterWheels env
      mvar = peDataMVar env
      startTime = peStartTime env
executeMeasurementElement (MEIrradiation dur ip) = executeIrradiation lss fws ip dur
    where
      lss = peLightSources env
      fws = peFilterWheels env
executeMeasurementElement _ (MEWait dur) = threadDelay (round $ dur * 1e6) >> return (Right ())
executeMeasurementElement env (MEDoTimes n es) = executeMeasurementElements . concat . take n . repeat $ es
executeMeasurementElement (METimeLapse n dur es) =
    forM_ futureMeasurements (\(ts, es) ->
        waitUntil ts >> executeMeasurementElements env es) >>
    return (Right ())
    where
        futureDurations = map (dur *) [0 .. (n - 1)]
        futureTimes = getTime Monotonic >>= \now ->
                      map (\delta -> fromNanoSecs (toNanoSecs now + round (delta * 1.0e9))) futureDurations
        futureMeasurements :: [(TimeSpec, [MeasurementElement])]
        futureMeasurements = futureTimes >>= \ft -> return (zip ft (repeat es))
        waitUntil :: TimeSpec -> IO ()
        waitUntil ts = getTime Monotonic >>= toNanoSecs . diffTimeSpec ts >>= \ns ->
                       threadDelay (fromIntegral (max (ns `div` 1000) 0))
executeMeasurementElement env (MEStageLoop sn poss) es) =
    forM_ poss (\pos -> setStagePosition sn pos >> executeMeasurementElements es) >>
    return (Right ())
    where
        stages = peMotorizedStages env
        setStagePosition :: Text -> StagePosition -> IO ()
        setStagePosition stageName pos = setStagePositionLookup stages stageName pos

executeDetection :: Detector a => a -> [LightSource] -> [FilterWheel] -> DetectionParams -> IO (Either String AcquiredData)
executeDetection det lss fws DetectionParams{..} =
    runExceptT (
        ExceptT (switchToFilters fws dpFilterParams) >>
        ExceptT (enableLightSources lss dpIrradiation) >>
        ExceptT (acquireData det dpExposureTime dpGain dpNSpectraToAverage) >>= \acquiredData ->
        ExceptT (disableLightSources lss dpIrradiation) >>
        ExceptT (return $ Right acquiredData))

executeIrradiation :: [LightSource] -> [FilterWheel] -> IrradiationParams -> Double -> IO (Either String ())
executeIrradiation lss fws ip dur =
    runExceptT (
        ExceptT (enableLightSources lightSources psIrradiation) >>
        ExceptT (Right <$> threadDelay (floor $ dur * 1.0e6)) >>
        ExceptT (disableLightSources lightSources psIrradiation))
    where

enableLightSources :: [LightSource] -> [IrradiationParams] -> IO (Either String ())
enableLightSources lss params
    | allLightSourcesKnown = catch (Right <$> mapM_ (\(IrradiationParams sourceName channel power _) -> activateLightSource (lookupLightSource lss sourceName) channel power) params)
                                (\e -> return (Left (displayException (e :: IOException))))
    | otherwise = return (Left "unknown light source")
    where
        allLightSourcesKnown = and $ map (isKnownLightSource lss . ipLightSourceName) params
disableLightSources :: [LightSource] -> [IrradiationParams] -> IO (Either String ())
disableLightSources lss params = catch (Right <$> mapM_ (\(IrradiationParams sourceName _ _) -> deactivateLightSource (lookupLightSource lss sourceName)) params)
                                    (\e -> return (Left (displayException (e :: IOException))))
    where
        allLightSourcesKnown = and $ map (isKnownLightSource lss . ipLightSourceName) params
