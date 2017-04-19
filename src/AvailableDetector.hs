{-# LANGUAGE ScopedTypeVariables #-}
module AvailableDetector where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Either
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Detector
#ifdef WITH_OCEANOPTICS
import OOSeaBreeze
import OODetector
#endif
#ifdef WITH_SCCAMERA
import SCCamera
import SCCamDetector
#endif
#ifdef WITH_DUMMYDETECTOR
import DummyDetector
#endif

import MiscUtils

#ifdef WITH_OCEANOPTICS
withAvailableDetector :: (OODetector -> IO ()) -> IO ()
withAvailableDetector f =
    getIDs >>= \(dID, fID, pfID) ->
    nonlinearityCorrection dID >>= \corrFunc ->
    withSeaBreeze (f (OODetector dID fID pfID Nothing corrFunc))
  where
      getIDs = getDeviceIDs >>= \ids ->
               when (null ids) (error "no spectrometer ids found") >>
               runExceptT (
                    ExceptT (openDevice (head ids)) >>
                    ExceptT (getSpectrometerFeatures (head ids)) >>= \(fID : _) ->
                    ExceptT (getSpectrumProcessingFeatures (head ids)) >>= \pfIDs ->
                    ExceptT (return (Right (head ids, fID, pfIDs)))) >>= \result ->
               case result of
                    Left _  -> error "no spectrometer found"
                    Right (dID, fID, pfIDs) ->
                        case pfIDs of
                            []         -> return (dID, fID, Nothing)
                            (pfID : _) -> return (dID, fID, Just pfID)
      nonlinearityCorrection dID =
            V.toList <$> getNonlinearityCoeffs dID >>= \coeffs ->
            return (\x -> x / polyCorr x coeffs)
            where
                polyCorr x coeffs = sum $ zipWith3 (\x coeff order -> coeff * x^order) (repeat x) coeffs ([0 ..] :: [Int])
#endif
#ifdef WITH_SCCAMERA
withAvailableDetector :: (SCCamDetector -> IO ()) -> IO ()
withAvailableDetector f =
  (bracket initializeCameraDLL (\_ -> shutdownCameraDLL) $ \initStatus ->
    when (isLeft initStatus) (error (fromLeft initStatus)) >>
    listConnectedCameras >>= \camNames ->
    when (null camNames) (
        putStrLn "no cameras found... press return to exit" >>
        getLine >> error "no cameras found") >>
    putStrLn ("using camera " ++ (T.unpack $ head camNames)) >>
    let camName = head camNames
    in f (SCCamDetector camName))
#endif
#ifdef WITH_DUMMYDETECTOR
withAvailableDetector :: (DummyDetector -> IO ()) -> IO ()
withAvailableDetector f = f DummyDetector
#endif
