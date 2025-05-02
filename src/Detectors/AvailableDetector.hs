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
import Camera.SCCamera
import Detectors.SCCamDetector

import Utils.MiscUtils

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
