{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import GPIO
import OOSeaBreeze
import SimpleJSONServer

import CuvettorTypes

main :: IO ()
main =
    withGPIOPins (zip availablePins (repeat $ Output Low)) $ \gpioPins ->
    
    withSeaBreeze $
    bracket fetchAvailableSpectrometer closeAvailableSpectrometer $ \maybeSpectrometer ->

    return (Environment gpioPins availablePins maybeSpectrometer) >>= \env ->
    runServer 3200 messageHandler env

messageHandler :: MessageHandler Environment
messageHandler msg env =
    case (fromJSON msg) of
        Error _   -> return (object [("responsetype", "invalidquery")], env)
        Success v -> performAction env v >>= \(resp, newEnv) -> return (toJSON resp, newEnv)

performAction :: Environment -> RequestMessage -> IO (ResponseMessage, Environment)
performAction _ (SetPinHigh pin) = undefined
performAction _ (SetPinLow pin) = undefined
performAction _ (AcquireSpectrum e n) = undefined
performAction env SendWavelengths = return (Wavelengths (V.fromList [0.0 .. 100.0]), env)

availablePins :: [GPIOPin]
availablePins = [Pin2]

fetchAvailableSpectrometer :: IO (Maybe (DeviceID, FeatureID))
fetchAvailableSpectrometer =
    getDeviceIDs >>= \idList ->
    if (not $ null idList)
      then
        runExceptT (
            ExceptT (openDevice (head idList)) >>
            ExceptT (getSpectrometerFeatures (head idList)) >>= \(featureID : _) ->
            return (head idList, featureID)) >>= \result ->
        case result of
            Left _  -> return Nothing
            Right v -> return $ Just v
      else return Nothing

closeAvailableSpectrometer :: Maybe (DeviceID, FeatureID) -> IO ()
closeAvailableSpectrometer Nothing              = return ()
closeAvailableSpectrometer (Just (deviceID, _)) = closeDevice deviceID >> return ()
