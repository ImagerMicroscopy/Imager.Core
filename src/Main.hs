module Main where

import Data.Aeson

import SimpleJSONServer

import CuvettorTypes
import CuvettorJSON

main :: IO ()
main = runServer 3200 messageHandler 0

messageHandler :: MessageHandler Int
messageHandler msg env = return (msg, 3)
    