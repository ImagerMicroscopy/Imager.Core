module Equipment.Devices.OxxiusLBX where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Format as T

import Equipment.Equipment
import Equipment.EquipmentTypes
import RCSerialPort

data OxxiusLBX = OxxiusLBX {
                    olbxName :: !EqName
                  , olbxPort :: !SerialPort
                  , maxPower :: Double
                 }

initializeOxxiusLBX :: EquipmentDescription -> IO EquipmentW
initializeOxxiusLBX desc =
    EquipmentW <$> (initializeOxxiusLBX' desc)

initializeOxxiusLBX' :: EquipmentDescription -> IO OxxiusLBX
initializeOxxiusLBX' (OxxiusLBXDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 1000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        oxxiusLBXInit port >>
        getMaxPower port >>= \power ->
        pure (OxxiusLBX (EqName name) port power)
    where
        oxxiusLBXInit :: SerialPort -> IO ()
        oxxiusLBXInit port = handleOxxiusLBXCommand port "L=0" >> -- turn off the laser just to make sure
                             handleOxxiusLBXCommand port "TTL=0" >> -- disable digital modulation
                             handleOxxiusLBXCommand port "ACC=0" >> -- regulate output power mode
                             handleOxxiusLBXCommand port "CW=1" -- constant power mode
        getMaxPower :: SerialPort -> IO Double
        getMaxPower port =
            T.decodeUtf8 <$> serialWriteAndReadUntilChar port "INF?\r\n" '\n' >>= \resp ->
            let [lasertTypeStr, wavelengthStr, powerStr] = T.split (== '-') resp
            in  pure (read (T.unpack powerStr))

instance Equipment OxxiusLBX where
    equipmentName (OxxiusLBX n _ _) = n
    flushSerialPorts (OxxiusLBX _ port _) = flushSerialPort port
    closeDevice ls@(OxxiusLBX _ port _) =
        deactivateLightSource ls >>
        closeSerialPort port
    availableLightSources _ =
        [LightSourceDescription (LSName "ls") True True [LSChannelName "ls"]]
    activateLightSource lbx@(OxxiusLBX _ port _) _ [(_, p)] =
        setRequestedPower lbx p >>
        handleOxxiusLBXCommand port "L=1"
    deactivateLightSource (OxxiusLBX _ port _) =
        handleOxxiusLBXCommand port "L=0"


setRequestedPower :: OxxiusLBX -> LSIlluminationPower -> IO ()
setRequestedPower (OxxiusLBX _ port maxP) (LSIlluminationPower p) =
    let actualP = (p / 100.0 * maxP)
        powerStr = LT.toStrict (T.format "{}.{}" (separateParts actualP)) -- Oxxius seems to like exactly one number after the comma
    in  handleOxxiusLBXCommand port ("PM=" <> powerStr)
    where
        separateParts :: Double -> (Int, Int)
        separateParts d = (round d, round (10.0 * (d - (fromIntegral (floor d)))))

handleOxxiusLBXCommand :: SerialPort -> Text -> IO ()
handleOxxiusLBXCommand port comm =
    let msg = comm <> "\r\n"
        expectedResponse = msg
    in  T.decodeUtf8 <$> serialWriteAndReadUntilChar port (T.encodeUtf8 msg) '\n' >>= \resp ->
        when (resp /= expectedResponse) (
            throwIO (userError ("unexpected response from Oxxius: sent " ++ T.unpack comm ++ " received " ++ T.unpack resp)))
