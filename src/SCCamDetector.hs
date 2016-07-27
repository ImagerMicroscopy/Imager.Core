{-# LANGUAGE BangPatterns, InstanceSigs #-}

module SCCamDetector where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign
import System.Clock

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
            ExceptT (setExposureTime camName expTime) >>
            ExceptT (setEMGain camName emGain) >>
            ExceptT (acquireImages camName 1)
        ) >>= \images ->
        case images of
            Left e  -> return (Left e)
            Right (MeasuredImages nRows nCols vec) ->
                let bytes = byteStringFromVector vec
                    numType = UINT16
                in return $ Right (AcquiredData nRows nCols bytes numType)
    acquireStreamingData :: SCCamDetector -> ExposureTime -> Gain -> NMeasurementsToAverage ->
                            NMeasurementsToPerform -> TimeSpec -> MVar ([[(AcquiredData, Double)]]) -> IO ()
    acquireStreamingData (SCCamDetector camName) expTime gain nMeasurementsToAverage nMeasurements startTime dataMVar =
        (flip finally) (abortAsyncAcquisition camName) (
            startAsyncAcquisition camName nImagesInBuffer >>= \status ->
            case status of
                Left e -> error e
                Right buffer ->
                    fetchImages (-1) nMeasurements buffer dataMVar
        )
        where
            fetchImages :: Int -> Int -> ImageBuffer -> MVar ([[(AcquiredData, Double)]]) -> IO ()
            fetchImages previousIndex nImagesRemaining buffer dataMVar
                | nImagesRemaining == 0 = return ()
                | otherwise =
                    getIndexOfLastImageAsyncAcquired camName >>= \ret ->
                    case ret of
                        Left e -> error e
                        Right i ->
                            if (i == previousIndex)
                            then threadDelay 5000 >> fetchImages previousIndex nImagesRemaining buffer dataMVar
                            else
                                 getTime Monotonic >>= \timeStamp ->
                                 getImageAtIndexInBuffer buffer i >>=
                                 return . measuredImagesAsAcquiredData >>= \dat ->
                                 addDataToMVar dataMVar startTime [(dat, timeStamp)] >>
                                 fetchImages i (nImagesRemaining - 1) buffer dataMVar
            measuredImagesAsAcquiredData :: MeasuredImages -> AcquiredData
            measuredImagesAsAcquiredData (MeasuredImages nRows nCols vec) =
                AcquiredData nRows nCols (byteStringFromVector vec) UINT16
                where
                    nImages = V.length vec `div` (nRows * nCols)
            nImagesInBuffer = 20
    getGainRange :: SCCamDetector -> IO (Either String (Gain, Gain))
    getGainRange (SCCamDetector camName) = readEMGainRange camName
    getExposureTimeRange :: SCCamDetector -> IO (Either String (ExposureTime, ExposureTime))
    getExposureTimeRange (SCCamDetector camName) = reaExposureTimeRange camName
