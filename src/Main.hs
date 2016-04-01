{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import SimpleJSONServer

import CuvettorTypes

main :: IO ()
main = runServer 3200 messageHandler Environment

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
