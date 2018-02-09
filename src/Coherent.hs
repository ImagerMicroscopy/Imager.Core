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

data Coherent = Coherent !EqName !SerialPort !(IORef (Bool, LSIlluminationPower, LSIlluminationPower)) !(IORef (Bool, LSIlluminationPower))

initializeCoherent :: EquipmentDescription -> IO EquipmentW
initializeCoherent (CoherentLightSourceDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 10000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        newIORef (False, LSIlluminationPower 0.0, LSIlluminationPower 0.0) >>= \powerRange ->
        newIORef (False, LSIlluminationPower 0.0) >>= \currentPower ->
        return (EquipmentW $ Coherent (EqName name) port powerRange currentPower)

instance Equipment Coherent where
    equipmentName (Coherent n _ _ _) = n
    flushSerialPorts (Coherent _ port _ _) = flushSerialPort port
    closeDevice (Coherent _ port _ _) = closeSerialPort port
    availableLightSources (Coherent n _ _ _) =
        [LightSourceDescription (LSName "ls") True False [LSChannelName "laser"]]
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
            setPower :: LSIlluminationPower -> IO ()
            setPower p = powerStr p >>= sendAndReadResponse >>
                         writeIORef currentPower (True, p)
            powerStr :: LSIlluminationPower -> IO ByteString
            powerStr (LSIlluminationPower p) =
                minMaxPower >>= \(LSIlluminationPower minPower, LSIlluminationPower maxPower) ->
                let powerVal = floor (minPower + p / 100.0 * (maxPower - minPower)) :: Int
                in return ("P=" <> T.encodeUtf8 (T.pack $ show powerVal) <> "\r")
            minMaxPower :: IO (LSIlluminationPower, LSIlluminationPower)
            minMaxPower = readIORef powerRange >>= \(haveRange, minP, maxP) ->
                          if (haveRange)
                          then return (minP, maxP)
                          else minPowerQ >>= \minPower -> maxPowerQ >>= \maxPower ->
                               writeIORef powerRange (True, minPower, maxPower) >>
                               sendAndReadResponse "CDRH=0\r" >> sendAndReadResponse "CW=1\r" >>
                               return (minPower, maxPower)
            minPowerQ = LSIlluminationPower <$> parseQuery "?MINLP\r"
            maxPowerQ = LSIlluminationPower <$> parseQuery "?MAXLP\r"
            parseQuery :: ByteString -> IO Double
            parseQuery q = sendAndReadResponse q >>= return . read . filter (`elem` ('.' : ['0' .. '9'])) . T.unpack . T.decodeUtf8
            sendAndReadResponse :: ByteString -> IO ByteString
            sendAndReadResponse msg = serialWriteAndReadUntilChar port msg '\n'
    deactivateLightSource (Coherent _ port _ _) =
        serialWriteAndReadUntilChar port "L=0\r" '\n' >> pure ()
