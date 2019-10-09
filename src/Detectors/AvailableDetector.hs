{-# LANGUAGE ScopedTypeVariables #-}

module Detectors.AvailableDetector where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Either hiding(fromLeft)
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Detectors.Detector
-- #ifdef WITH_OCEANOPTICS
-- import OOSeaBreeze
-- import OODetector
-- #endif
-- #ifdef WITH_SCCAMERA
import Camera.SCCamera
import Detectors.SCCamDetector
-- #endif
-- #ifdef WITH_DUMMYDETECTOR
-- import DummyDetector
-- #endif

import Utils.MiscUtils

-- #ifdef WITH_OCEANOPTICS
-- withAvailableDetector :: (OODetector -> IO ()) -> IO ()
-- withAvailableDetector f =
--     getIDs >>= \(dID, fID, pfID) ->
--     nonlinearityCorrection dID >>= \corrFunc ->
--     withSeaBreeze (f (OODetector dID fID pfID Nothing corrFunc))
--   where
--       getIDs = getDeviceIDs >>= \ids ->
--                when (null ids) (error "no spectrometer ids found") >>
--                openDevice (head ids) >>
--                getSpectrometerFeatures (head ids) >>= \(fID : _) ->
--                getSpectrumProcessingFeatures (head ids) >>= \pfIDs ->
--                return (head ids, fID, pfIDs) >>= \result ->
--                case pfIDs of
--                    []         -> return (head ids, fID, Nothing)
--                    (pfID : _) -> return (head ids, fID, Just pfID)
--       nonlinearityCorrection dID =
--             V.toList <$> getNonlinearityCoeffs dID >>= \coeffs ->
--             return (\x -> x / polyCorr x coeffs)
--             where
--                 polyCorr x coeffs = sum $ zipWith3 (\x coeff order -> coeff * x^order) (repeat x) coeffs ([0 ..] :: [Int])
-- #endif
-- #ifdef WITH_SCCAMERA
withAvailableDetectors :: ([SCCamDetector] -> IO ()) -> IO ()
withAvailableDetectors f =
    (bracket initializeCameraDLL (\_ -> shutdownCameraDLL) $ \initStatus ->
    when (isLeft initStatus) (error (fromLeft initStatus)) >>
    listConnectedCameras >>= \camNames ->
    when (null camNames) (
        putStrLn "no cameras found... press return to exit" >>
        getLine >> error "no cameras found") >>
    forM_ camNames (\cn -> putStrLn ("using camera " ++ (T.unpack cn))) >>
    f (map SCCamDetector camNames))
-- #endif
-- #ifdef WITH_DUMMYDETECTOR
-- withAvailableDetector :: (DummyDetector -> IO ()) -> IO ()
-- withAvailableDetector f = f DummyDetector
-- #endif
