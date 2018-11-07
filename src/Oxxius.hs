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

data OxxiusLC = OxxiusLC !EqName !SerialPort ![(LSChannelName, OxxiusLaserParams)]

data OxxiusLaserParams = OxxiusLaserParams {
                             oxxType :: !OxxiusLaserType
                           , oxxMaxPower :: !Double
                           , oxxIndex :: !Int -- index in combiner
                         }
                       deriving (Eq, Show)

data OxxiusLaserType = LBX | LCX
                      deriving (Eq, Show)

initializeOxxiusLC :: EquipmentDescription -> IO EquipmentW
initializeOxxiusLC (OxxiusLCDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 1000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        handleOxxiusCombinerOKCommand port "SH1=1" >> -- open shutter 1
        getLaserDetails port >>= \lasers ->
        forM_ (map snd lasers) (\(OxxiusLaserParams lType _ idx) ->
            handleOxxiusLaserCommand port idx "CDRH=0" >>
            handleOxxiusLaserCommand port idx "T=1" >>
            oxxiusTypeSpecificInit port lType idx) >>
        pure (EquipmentW $ OxxiusLC (EqName name) port lasers)
    where
        getLaserDetails :: SerialPort -> IO [(LSChannelName, OxxiusLaserParams)]
        getLaserDetails port = concat <$> mapM (getLaserDetail port) [1 .. 6]
        getLaserDetail :: SerialPort -> Int -> IO [(LSChannelName, OxxiusLaserParams)]
        getLaserDetail port idx =
            let msg = T.encodeUtf8 . LT.toStrict $ T.format "L{} INF?\r\n" (T.Only idx)
            in  T.decodeUtf8 <$> serialWriteAndReadUntilChar port msg '\n' >>= \resp ->
                if ("timeout" `T.isPrefixOf` resp)
                then pure []
                else
                    let laserType = case (T.take 3 resp) of
                                       "LBX" -> LBX
                                       "LCX" -> LCX
                                       v     -> throw $ userError ("unknown oxxius lasertype " ++ T.unpack v)
                        params = OxxiusLaserParams laserType (extractMaxPower resp) idx
                    in  pure [(LSChannelName resp, params)]
            where
              extractMaxPower :: Text -> Double
              extractMaxPower = read . T.unpack . T.reverse . T.takeWhile ((/=) '-') . T.reverse
        oxxiusTypeSpecificInit :: SerialPort -> OxxiusLaserType -> Int -> IO ()
        oxxiusTypeSpecificInit port LCX idx =
            let msg = LT.toStrict (T.format "L{} {}\r\n" (idx, ("DL=1" :: Text)))
            in  handleOxxiusCombinerOKCommand port msg
        oxxiusTypeSpecificInit _ _ _ = pure ()

instance Equipment OxxiusLC where
    equipmentName (OxxiusLC n _ _) = n
    flushSerialPorts (OxxiusLC _ port _) = flushSerialPort port
    closeDevice (OxxiusLC _ port chs) =
        forM_ (map (oxxIndex . snd) chs) (\idx -> handleOxxiusLaserCommand port idx "L=0") >>
        handleOxxiusCombinerOKCommand port "SH1 0" >> -- close shutter 1
        closeSerialPort port
    availableLightSources (OxxiusLC n _ chs) =
        [LightSourceDescription (LSName "ls") True True (map fst chs)]
    activateLightSource olc@(OxxiusLC _ port availChs) _ chs =
        setRequestedPowers port availChs chs >>
        activateLasers port availChs chs
        where
            setRequestedPowers :: SerialPort -> [(LSChannelName, OxxiusLaserParams)] -> [(LSChannelName, LSIlluminationPower)] -> IO ()
            setRequestedPowers port availChs chs =
                forM_ chs (\(chName, LSIlluminationPower p) ->
                    let params = fromJust (lookup chName availChs)
                    in  setLaserPower port params p)
            setLaserPower :: SerialPort -> OxxiusLaserParams -> Double -> IO ()
            setLaserPower port (OxxiusLaserParams lType maxP idx) p =
                let actualP = (p / 100.0 * maxP)
                    powerStr = LT.toStrict (T.format "{}.{}" (separateParts actualP)) -- Oxxius seems to like exactly one number after the comma
                in  case lType of
                        LBX -> handleOxxiusLaserCommand port idx ("PM=" <> powerStr)
                        LCX -> handleOxxiusCombinerEchoCommand port ("P=" <> powerStr)
                where
                    separateParts :: Double -> (Int, Int)
                    separateParts d = (round d, round (10.0 * (d - (fromIntegral (floor d)))))
            activateLasers port availChs chs =
                forM_ chs (\(chName, LSIlluminationPower p) ->
                    let cmd = "L=1"
                        idx = oxxIndex . fromJust $ (lookup chName availChs)
                    in  handleOxxiusLaserCommand port idx cmd)
    deactivateLightSource (OxxiusLC _ port chs) =
        forM_ (map snd chs) (\(OxxiusLaserParams lType _ idx) ->
            case lType of
                LBX -> handleOxxiusLaserCommand port idx "L=0"
                LCX -> handleOxxiusCombinerEchoCommand port "P=0.0")

handleOxxiusCombinerOKCommand :: SerialPort -> Text -> IO ()
handleOxxiusCombinerOKCommand port cmd =
    let expectedResponse = "OK\r\n"
    in  handleOxxiusCombinerCommand port cmd expectedResponse

handleOxxiusCombinerEchoCommand :: SerialPort -> Text -> IO ()
handleOxxiusCombinerEchoCommand port cmd =
    let expectedResponse = cmd <> "\r\n"
    in  handleOxxiusCombinerCommand port cmd expectedResponse

handleOxxiusCombinerCommand :: SerialPort -> Text -> Text -> IO ()
handleOxxiusCombinerCommand port cmd expectedResponse =
    T.decodeUtf8 <$> serialWriteAndReadUntilChar port (T.encodeUtf8 cmd <> "\n") '\n' >>= \resp ->
    when (resp /= expectedResponse) (
        throwIO (userError ("unexpected response from Oxxius: sent " ++ T.unpack cmd ++ " received " ++ T.unpack resp)))

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
