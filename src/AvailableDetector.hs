{-# LANGUAGE ScopedTypeVariables #-}
module AvailableDetector where

import Control.Monad
import Control.Monad.Trans.Except
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

#if !defined(WITH_OCEANOPTICS) && !defined(WITH_SCCAMERA)
    #error "building without any detector support"
#endif
#if defined(WITH_OCEANOPTICS) && defined(WITH_SCCAMERA)
    #error "must build for either camera or spectrometer"
#endif

#ifdef WITH_OCEANOPTICS
withAvailableDetector :: (OODetector -> IO ()) -> IO ()
withAvailableDetector f =
    getIDs >>= \(dID, fID) ->
    nonlinearityCorrection dID >>= \corrFunc ->
    withSeaBreeze (f (OODetector dID fID Nothing corrFunc))
  where
      getIDs = getDeviceIDs >>= \ids ->
               when (null ids) (error "no spectrometer ids found") >>
               runExceptT (
                    ExceptT (openDevice (head ids)) >>
                    ExceptT (getSpectrometerFeatures (head ids)) >>= \(featureID : _) ->
                    return (head ids, featureID)) >>= \result ->
               case result of
                    Left _  -> error "no spectrometer found"
                    Right (dID, fID) -> return (dID, fID)
      nonlinearityCorrection dID =
            V.toList <$> getNonlinearityCoeffs dID >>= \coeffs ->
            return (\x -> x / polyCorr x coeffs)
            where
                polyCorr x coeffs = sum $ zipWith3 (\x coeff order -> coeff * x^order) (repeat x) coeffs ([0 ..] :: [Int])
#endif
#if WITH_SCCAMERA
withAvailableDetector :: (SCCamDetector -> IO ()) -> IO ()
withAvailableDetector f =
  (bracket initializeCameraDLL (\_ -> shutdownCameraDLL) $ \initStatus ->
    when (isLeft initStatus) (error (fromLeft initStatus)) >>
    listConnectedCameras >>= \camNames ->
    when (null camNames) (error "no cameras found") >>
    putStrLn ("using camera " ++ (T.unpack $ head camNames)) >>
    let camName = head camNames
    in f (SCCamDetector camName)
#endif
