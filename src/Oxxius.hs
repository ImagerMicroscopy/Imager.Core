module Oxxius where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Format as T

import Equipment
import EquipmentTypes
import RCSerialPort

data OxxiusLC = OxxiusLC !EqName !SerialPort ![(LSChannelName, Int)]

initializeOxxiusLC :: EquipmentDescription -> IO EquipmentW
initializeOxxiusLC (OxxiusLCDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 1000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        handleOxxiusCombinerCommand port "SH1 1" >> -- open shutter 1
        handleOxxiusCombinerCommand port "ACC 0" >>
        getLaserDetails port >>= \lasers ->
        forM_ (map snd lasers) (\idx ->
            handleOxxiusLaserCommand port idx "CDRH=0" >>
            handleOxxiusLaserCommand port idx "ACC=0" >>
            handleOxxiusLaserCommand port idx "CW=1") >>
        pure (EquipmentW $ OxxiusLC (EqName name) port lasers)
    where
        getLaserDetails :: SerialPort -> IO [(LSChannelName, Int)]
        getLaserDetails port = concat <$> mapM (getLaserDetail port) [1 .. 6]
        getLaserDetail :: SerialPort -> Int -> IO [(LSChannelName, Int)]
        getLaserDetail port idx =
            (handleOxxiusLaserQuery port idx "INF?" >>= \info ->
                return [(LSChannelName info, idx)]) `catch` (\e -> pure (e :: IOException) >> pure [])

instance Equipment OxxiusLC where
    equipmentName (OxxiusLC n _ _) = n
    flushSerialPorts (OxxiusLC _ port _) = flushSerialPort port
    closeDevice (OxxiusLC _ port _) = handleOxxiusCombinerCommand port "SH1 0" >> -- close shutter 1
                                      closeSerialPort port
    availableLightSources (OxxiusLC n _ chs) =
        [LightSourceDescription (LSName "ls") True True (map fst chs)]
    activateLightSource olc@(OxxiusLC _ port availChs) _ chs =
        setRequestedPowers port availChs chs >>
        activateLasers port availChs chs
        where
            setRequestedPowers :: SerialPort -> [(LSChannelName, Int)] -> [(LSChannelName, LSIlluminationPower)] -> IO ()
            setRequestedPowers port availChs chs =
                forM_ chs (\(chName, LSIlluminationPower p) ->
                    let cmd = LT.toStrict (T.format "IP={}" (T.Only p))
                        idx = fromJust (lookup chName availChs)
                    in  handleOxxiusLaserCommand port idx cmd)
            activateLasers port availChs chs =
                forM_ chs (\(chName, LSIlluminationPower p) ->
                    let cmd = "L=1"
                        idx = fromJust (lookup chName availChs)
                    in  handleOxxiusLaserCommand port idx cmd)
    deactivateLightSource (OxxiusLC _ port chs) =
        forM_ (map snd chs) (\idx ->
            handleOxxiusLaserCommand port idx "L=0")

handleOxxiusCombinerCommand :: SerialPort -> Text -> IO ()
handleOxxiusCombinerCommand port comm =
    let expectedResponse = "OK\r\n"
    in  T.decodeUtf8 <$> serialWriteAndReadUntilChar port (T.encodeUtf8 comm <> "\n") '\n' >>= \resp ->
        when (resp /= expectedResponse) (
            throwIO (userError ("unexpected response from Oxxius: sent " ++ T.unpack comm ++ " received " ++ T.unpack resp)))

handleOxxiusLaserQuery :: SerialPort -> Int -> Text -> IO Text
handleOxxiusLaserQuery port idx q =
    let msg = LT.toStrict (T.format "L{} {}\r\n" (idx, q))
    in  handleOxxiusQuery port msg
    where
        handleOxxiusQuery :: SerialPort -> Text -> IO Text
        handleOxxiusQuery port q =
            (T.breakOn "=" . T.decodeUtf8) <$> serialWriteAndReadUntilChar port (T.encodeUtf8 q) '\n' >>= \(_, resp) ->
            pure . T.drop 1 . T.take (T.length resp - 2) $ resp

handleOxxiusLaserCommand :: SerialPort -> Int -> Text -> IO ()
handleOxxiusLaserCommand port idx comm =
    let expectedResponse = comm <> "\r\n"
        msg = LT.toStrict (T.format "L{} {}\r\n" (idx, comm))
    in  T.decodeUtf8 <$> serialWriteAndReadUntilChar port (T.encodeUtf8 msg) '\n' >>= \resp ->
        when (resp /= expectedResponse) (
            throwIO (userError ("unexpected response from Oxxius: sent " ++ T.unpack comm ++ " received " ++ T.unpack resp)))
