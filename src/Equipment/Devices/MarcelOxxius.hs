module Equipment.Devices.MarcelOxxius where

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
import Equipment.Devices.Oxxius
import RCSerialPort

data MarcelOxxiusLC = MarcelOxxiusLC {
                          molcOx :: !OxxiusLC
                        , molcArduinoPort :: !SerialPort
                    }

data MarcelOxxiusChannels = MOCViolet
                          | MOCBlue
                          | MOCCyan
                          | MOCGreen
                          | MOCRed
                          deriving (Eq, Show)

data MarcelOxxiusIlluminationMode = MOMGated
                                  | MOMTimed !LSIlluminationDuration

initializeMarcelOxxiusLC :: EquipmentDescription -> IO EquipmentW
initializeMarcelOxxiusLC (MarcelOxxiusLCDesc name oxxPortName ardPortName) =
    let arSerialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 60000) SerialPortNoDebug
    in  initializeOxxiusLC' (OxxiusLCDesc name oxxPortName DigitalModulation) >>= \(ox@(OxxiusLC _ oxPort lasers _)) ->
        openSerialPort ardPortName arSerialSettings >>= \arPort -> threadDelay 2000000 >>
        handleOxxiusCombinerEchoCommand oxPort "AM=1" >> -- enable analog modulation
        forM_ (map snd lasers) (\laserParams ->
            enableAnalogMode oxPort laserParams.oxxType laserParams.oxxIndex) >>
        pure (EquipmentW (MarcelOxxiusLC ox arPort))
        where
            enableAnalogMode :: SerialPort -> OxxiusLaserType -> Int -> IO ()
            enableAnalogMode port LBX idx = handleOxxiusLaserCommand port idx "AM=1" -- enable analog modulation
            enableAnalogMode _ _ _ = pure ()

instance Equipment MarcelOxxiusLC where
    equipmentName (MarcelOxxiusLC ox _) = equipmentName ox
    flushSerialPorts (MarcelOxxiusLC ox arPort) = flushSerialPorts ox >> flushSerialPort arPort
    closeDevice (MarcelOxxiusLC ox arPort) = closeDevice ox >> closeSerialPort arPort
    availableLightSources (MarcelOxxiusLC ox _) = availableLightSources ox
    activateLightSource mox@(MarcelOxxiusLC ox _) n chs =
        let msg = marcelOxxiusIlluminationMessage mox chs MOMGated
        in  activateLightSource ox n chs >> handleMarcelOxxiusMessage mox msg
    activateLightSourceTimed mox@(MarcelOxxiusLC ox _) n chs dur =
        let msg = marcelOxxiusIlluminationMessage mox chs (MOMTimed dur)
        in  activateLightSource ox n chs >> handleMarcelOxxiusMessage mox msg
    deactivateLightSource mox@(MarcelOxxiusLC ox ardPort) =
      handleMarcelOxxiusMessage mox "x" >> deactivateLightSource ox

marcelOxxiusIlluminationMessage :: MarcelOxxiusLC -> [(LSChannelName, LSIlluminationPower)] -> MarcelOxxiusIlluminationMode -> ByteString
marcelOxxiusIlluminationMessage mox chs mode =
    let availableLasers = olcLasers . molcOx $ mox
        requestedLaserNames = map fst chs
        laserParams = map snd . filter (\c -> (fst c) `elem` requestedLaserNames) $ availableLasers
        codes = map marcelOxxiusCodeForLaser laserParams
        fullMsg = case mode of
                      MOMGated      -> channelMsgs "30000"
                      MOMTimed dur  -> let durationUS = max (10 :: Int) (round (1.0e6 * fromLSIlluminationDuration dur))
                                       in  T.encodeUtf8 $ LT.toStrict $ T.format "irr:{}:0:i {}" (durationUS, T.decodeUtf8 (channelMsgs "1"))
        channelMsgs nReps = B.intercalate " " (map (\c -> c <> nReps) codes)
    in  fullMsg
    where
        marcelOxxiusCodeForLaser :: OxxiusLaserParams -> ByteString
        marcelOxxiusCodeForLaser (OxxiusLaserParams _ _ wl _ _ _)
            | within wl 380 420 = "V"
            | within wl 420 470 = "B"
            | within wl 470 520 = "C"
            | within wl 520 600 = "G"
            | within wl 600 660 = "R"
            | otherwise         = error ("No oxxius code for wavelength " ++ show wl)
            where
                within :: (Ord a) => a -> a -> a -> Bool
                within a b c = (a >= b) && (a <= c)

handleMarcelOxxiusMessage :: MarcelOxxiusLC -> ByteString -> IO ()
handleMarcelOxxiusMessage (MarcelOxxiusLC _ arPort) bs =
    serialWriteAndReadUntilChar arPort (bs <> "\r\n") '\n' >>= \resp ->
    when (B.take 2 resp /= "OK") (throwIO $ userError "Error response from MarcelOxxius Arduino")
