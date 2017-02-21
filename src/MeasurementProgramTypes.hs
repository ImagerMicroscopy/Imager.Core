{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module MeasurementProgramTypes where

import Data.Aeson
import Data.Text (Text)

type StagePosition = (Double, Double, Double)

data MeasurementElement = MEDetection [DetectionParams]
                        | MEIrradiation Double IrradiationParams
                        | MEWait Double
                        | MEDoTimes Int [MeasurementElement]
                        | METimeLapse Int Double [MeasurementElement]
                        | MEStageLoop Text [StagePosition] [MeasurementElement]
                        deriving (Show)

data ProgramEnvironment a = ProgramEnvironment {
                                peDetector :: a
                              , peStartTime :: TimeSpec
                              , peLightSources :: [LightSource]
                              , peFilterWheels :: [FilterWheel]
                              , peMotorizedStages :: [MotorizedStage]
                              , peDataMVar :: MVar [[AcquiredData]]
                            }

data DetectionParams = DetectionParams {
                           dpExposureTime :: !Double
                         , dpGain :: !Double
                         , dpNSpectraToAverage :: !Int
                         , dpIrradiation :: [IrradiationParams]
                         , dpFilterParams :: [FilterParams]
                       }

data IrradiationParams = IrradiationParams {
                            ipLightSourceName :: !Text
                          , ipLightSourceChannel :: ![Text]
                          , ipPower :: ![Double]
                        }
                        deriving (Show)

data FilterParams = FilterParams {
                        fpFilterWheelName :: !Text
                      , fpFilterName :: !Text
                    } deriving (Show)

instance FromJSON MeasurementElement where
    parseJSON (Object v) =
        v .: "elementtype" >>= \type ->
        case type of
          "detection"   -> MEDetection <$> v .: "detection"
          "irradiation" -> MEIrradiation <$> v .: "duration" <*> v .: "irradiation"
          "wait"        -> MEWait <$> v .: "duration"
          "dotimes"     -> MEDoTimes <$> v .: "ntotal" <*> v .: "elements"
          "timelapse"   -> METimeLapse <$> v .: "ntotal" <*> v .: "timedelta" <*> "elements"
          "stageloop"   -> MEStageLoop <$> v .: "stagename" <*> v .: "positions" <*> v .: "elements"
    parseJSON = fail "can't decode measurement element"
instance toJSON MeasurementElement where
  toEncoding (MEDetection dets) = pairs ("elementtype" .= "detection" <> "detection" .= dets)
  toEncoding (MEIrradiation dur ip) = pairs ("elementtype" .= "irradiation" <> "duration" .= dur <> "irradiation" .= dets)
  toEncoding (MEWait d) = pairs ("elementtype" .= "wait" <> "duration" <> d)
  toEncoding (MEDoTimes n es) = pairs ("elementtype" .= "dotimes" <> "nototal" .= n <> "elements" .= es)
  toEncoding (METimeLapse n td es) = pairs ("elementtype" .= "timelapse" <> "ntotal" .= n <> "timedelta" .= td <> "elements" .= es)
  toEncoding (METimeLapse n td es) = pairs ("elementtype" .= "timelapse" <> "ntotal" .= n <> "timedelta" .= td <> "elements" .= es)
  toEncoding (MEStageLoop n pos es) = pairs ("elementtype" .= "stageloop" <> "stagename" .= n <> "positions" .= pos <> "elements" .= es)

instance FromJSON DetectionParams where
    parseJSON (Object v) =
        DetectionParams <$> v .: "exposuretime"
                        <*> v .: "gain"
                        <*> v .: "nspectra"
                        <*> v .: "irradiation"
                        <*> v .: "filters"
    parseJSON _ = fail "can't decode detection params"
instance ToJSON DetectionParams where
    toEncoding (DetectionParams expTime gain nSpectra irr filters) =
        pairs ("exposuretime" .= expTime <> "gain" .= gain <> "nspectra" .= nSpectra <> "irradiation" .= irr <> "filters" .= filters)
    toJSON _ = error "no toJSON"

instance FromJSON IrradiationParams where
    parseJSON (Object v) =
        IrradiationParams <$> v .: "lightsourcename"
                          <*> v .: "lightsourcechannel"
                          <*> v .: "lightsourcepower"
    parseJSON _ = fail "can't decode irradiation params"
instance ToJSON IrradiationParams where
    toEncoding (IrradiationParams lName channel power) =
        pairs ("lightsourcename" .= lName <> "lightsourcechannel" .= channel <> "lightsourcepower" .= power)
    toJSON _ = error "no toJSON"

instance FromJSON FilterParams where
    parseJSON (Object v) =
        FilterParams <$> v .: "filterwheelname"
                          <*> v .: "filtername"
    parseJSON _ = fail "can't decode irradiation params"
instance ToJSON FilterParams where
    toEncoding (FilterParams filterWheelName filterName) =
        pairs ("filterwheelname" .= filterWheelName <> "filtername" .= filterName)
    toJSON _ = error "no toJSON"
