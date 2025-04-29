module Equipment.Devices.Marzhauser where

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

import Equipment.Equipment
import Equipment.EquipmentTypes
import RCSerialPort

data MarzhauserStage = MarzhauserStage {
                              mhsName :: !EqName
                            , mhsPort :: !SerialPort
                            , mhsAxes :: ![StageAxis]
                            }

initializeMarzhauserStage :: EquipmentDescription -> IO EquipmentW
initializeMarzhauserStage (MarzhauserStageDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS57600, stopb = Two}) (TimeoutMillis 30000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        sendMarzhauserMessageNoResponse port "!dim 1 1 1" >>
        sendMarzhauserMessageNoResponse port "!autostatus 0" >> -- we will poll for move completion
        determineAvailableAxes port >>= \axes ->
        return (EquipmentW (MarzhauserStage (EqName name) port axes))

instance Equipment MarzhauserStage where
    equipmentName = mhsName
    flushSerialPorts mh = flushSerialPort (mhsPort mh)
    closeDevice mh = closeSerialPort (mhsPort mh)
    hasMotorizedStage _ = True
    motorizedStageName _ = StageName "stage"
    supportedStageAxes mh = mh.mhsAxes
    getStagePosition (MarzhauserStage _ port axes) =
        map read . words . T.unpack . T.decodeUtf8 <$> handleMarzhauserMessage port "?pos" >>= \coords ->
        if (length axes == 2)
        then pure (StagePosition (coords !! 0) (coords !! 1) (-1.0) False 0)
        else pure (StagePosition (coords !! 0) (coords !! 1) (coords !! 2) False 0)
    setStagePosition (MarzhauserStage _ port _) (StagePosition x y z _ _) =
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

determineAvailableAxes :: SerialPort -> IO [StageAxis]
determineAvailableAxes port =
    handleMarzhauserMessage port "?axis" >>= \resp ->
    case (length . words . T.unpack . T.decodeUtf8 $ resp) of
        2 -> pure [XAxis, YAxis]
        3 -> pure [XAxis, YAxis, ZAxis]
        _ -> throwIO $ userError "can't determine number of Marzhauser axes"

handleMarzhauserMessage :: SerialPort -> ByteString -> IO ByteString
handleMarzhauserMessage port bs =
    serialWriteAndReadUntilChar port (bs <> "\r") '\r'

sendMarzhauserMessageNoResponse :: SerialPort -> ByteString -> IO ()
sendMarzhauserMessageNoResponse port bs = serialWrite port (bs <> "\r")
