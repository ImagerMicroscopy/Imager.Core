module Marzhauser where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List
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
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS57600}) (TimeoutMillis 30000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        handleMarzhauserMessage port "!dim 2" >>= \resp ->
        when (resp /= "2\r") (throwIO $ userError "unexpected response from Marzhauser stage") >>
        return (EquipmentW (MarzhauserStage (EqName name) port))

instance Equipment MarzhauserStage where
    equipmentName (MarzhauserStage n _) = n
    flushSerialPorts (MarzhauserStage _ port) = flushSerialPort port
    closeDevice (MarzhauserStage _ port) = closeSerialPort port
    hasMotorizedStage _ = True
    motorizedStageName _ = StageName "stage"
    getStagePosition (MarzhauserStage _ port) =
        map read . words . T.unpack . T.decodeUtf8 <$> handleMarzhauserMessage port "?pos" >>= \[x, y, z] ->
        pure (x, y, z)
    setStagePosition (MarzhauserStage _ port) (x, y, z) =
        let msg = LT.toStrict $ format "pos {} {} {}" (x, y, z)
        in  handleMarzhauserMessage port (T.encodeUtf8 msg) >> pure ()

handleMarzhauserMessage :: SerialPort -> ByteString -> IO ByteString
handleMarzhauserMessage port bs = serialWrite port (B.append bs "\r") >>
                                  serialReadUntilChar port '\r'
