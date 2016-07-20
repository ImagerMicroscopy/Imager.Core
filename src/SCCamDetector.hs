{-# LANGUAGE BangPatterns, InstanceSigs #-}

module SCCamDetector where

import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign

import Detector
import SCCamera
import MiscUtils

data SCCamDetector = SCCamDetector {
                         sccCamName :: !Text
                     }

instance Detector SCCamDetector where
    acquireData :: SCCamDetector -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO (Either String AcquiredData)
    acquireData (SCCamDetector camName) expTime emGain _ =
        runExceptT (
            setExposureTime camName expTime >>
            setEMGain camName emGain >>
            acquireImages camName 1
        ) >>= \images ->
        case images of
            Left e  -> return (Left e)
            Right (MeasuredImages nRows nCols vec) ->
                let bytes = byteStringFromVector Vector
                    numType = UINT16
                in return $ Right (AcquiredData nRows nCols bytes numType)
    getGainRange :: SCCamDetector -> IO (Either String (Gain, Gain))
    getGainRange (SCCamDetector camName) = readEMGainRange camName
    getExposureTimeRange :: SCCamDetector -> IO (Either String (ExposureTime, ExposureTime))
    getExposureTimeRange (SCCamDetector camName) = reaExposureTimeRange camName
