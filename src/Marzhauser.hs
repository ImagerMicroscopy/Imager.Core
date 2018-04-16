module Marzhauser where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Data.Text.Format

import Equipment
import EquipmentTypes
import RCSerialPort

data MarzhauserStage = MarzhauserStage !EqName !SerialPort

initializeMarzhauserStage :: EquipmentDescription -> IO EquipmentW
initializeMarzhauserStage (MarzhauserStageDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS57600, stopb = Two}) (TimeoutMillis 30000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        sendMarzhauserMessageNoResponse port "!dim 1 1 1" >>
        sendMarzhauserMessageNoResponse port "!autostatus 0" >> -- we will poll for move completion
        return (EquipmentW (MarzhauserStage (EqName name) port))

instance Equipment MarzhauserStage where
    equipmentName (MarzhauserStage n _) = n
    flushSerialPorts (MarzhauserStage _ port) = flushSerialPort port
    closeDevice (MarzhauserStage _ port) = closeSerialPort port
    hasMotorizedStage _ = True
    motorizedStageName _ = StageName "stage"
    supportedStageAxes _ = [XAxis, YAxis, ZAxis]
    getStagePosition (MarzhauserStage _ port) =
        map read . words . T.unpack . T.decodeUtf8 <$> handleMarzhauserMessage port "?pos" >>= \[x, y, z] ->
        pure (x, y, z)
    setStagePosition (MarzhauserStage _ port) (x, y, z) =
        doMove `onException` abortMovement
        where
            doMove = sendMarzhauserMessageNoResponse port (T.encodeUtf8 msg) >>
                     waitUntilMovementStops
            msg = LT.toStrict $ format "!moa {} {} {}" (x, y, z)
            waitUntilMovementStops =
                handleMarzhauserMessage port "?statusaxis" >>= \resp ->
                if (any (== (fromIntegral $ fromEnum 'M')) (B.unpack (B.take 3 resp)))
                then threadDelay 20000 >> waitUntilMovementStops
                else pure ()
            abortMovement = sendMarzhauserMessageNoResponse port "a -1"

handleMarzhauserMessage :: SerialPort -> ByteString -> IO ByteString
handleMarzhauserMessage port bs =
    serialWriteAndReadUntilChar port (bs <> "\r") '\r'

sendMarzhauserMessageNoResponse :: SerialPort -> ByteString -> IO ()
sendMarzhauserMessageNoResponse port bs = serialWrite port (bs <> "\r")
