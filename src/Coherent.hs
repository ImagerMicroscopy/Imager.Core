{-# LANGUAGE OverloadedStrings #-}
module Coherent where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Either
import Data.Set (Set)
import qualified Data.Set as S
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word

import Equipment
import EquipmentTypes
import MiscUtils
import RCSerialPort

data Coherent = Coherent !LSName !SerialPort !(IORef (Bool, Double, Double)) !(IORef (Bool, Double))

initializeCoherent :: EquipmentDescription -> IO EquipmentW
initializeCoherent (CoherentLightSourceDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        newIORef (False, 0.0, 0.0) >>= \powerRange ->
        newIORef (False, 0.0) >>= \currentPower ->
        return (EquipmentW $ Coherent (LSName name) port powerRange currentPower)

instance Equipment Coherent where
    equipmentName _ = (EqName "Coherent laser")
    closeDevice (Coherent _ port _ _) = closeSerialPort port
    availableLightSources (Coherent n _ _ _) =
        [LightSourceDescription n True False [LSChannelName "laser"]]
    activateLightSource (Coherent _ port powerRange currentPower) _ ((_, power) : _) =
        needToSetPower >>= \needPower ->
        when (needPower) (setPower power) >>
        sendAndReadResponse "L=1\r" >> return ()
        where
            needToSetPower :: IO Bool
            needToSetPower =
                readIORef currentPower >>= \cp ->
                case cp of
                    (False, _) -> return True
                    (True, p)  -> return (p /= power)
            setPower :: Double -> IO ()
            setPower p = powerStr p >>= sendAndReadResponse >>
                         writeIORef currentPower (True, p)
            powerStr :: Double -> IO ByteString
            powerStr p = minMaxPower >>= \(minPower, maxPower) ->
                        let powerVal = floor (minPower + p / 100.0 * (maxPower - minPower)) :: Int
                        in return ("P=" <> T.encodeUtf8 (T.pack $ show powerVal) <> "\r")
            minMaxPower :: IO (Double, Double)
            minMaxPower = readIORef powerRange >>= \(haveRange, minP, maxP) ->
                          if (haveRange)
                          then return (minP, maxP)
                          else minPowerQ >>= \minPower -> maxPowerQ >>= \maxPower ->
                               writeIORef powerRange (True, minPower, maxPower) >>
                               sendAndReadResponse "CDRH=0\r" >> sendAndReadResponse "CW=1\r" >>
                               return (minPower, maxPower)
            minPowerQ = parseQuery "?MINLP\r"
            maxPowerQ = parseQuery "?MAXLP\r"
            parseQuery :: ByteString -> IO Double
            parseQuery q = sendAndReadResponse q >>= return . read . filter (`elem` ('.' : ['0' .. '9'])) . T.unpack . T.decodeUtf8
            sendAndReadResponse :: ByteString -> IO ByteString
            sendAndReadResponse msg = flushSerialPort port >> serialWrite port msg >> serialReadUntilChar port '\n'
    deactivateLightSource (Coherent _ port _ _) =
        flushSerialPort port >> serialWrite port "L=0\r" >> serialReadUntilChar port '\n' >> return ()
