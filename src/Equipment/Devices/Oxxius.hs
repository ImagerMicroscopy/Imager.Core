module Equipment.Devices.Oxxius where

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
import Utils.MiscUtils
import RCSerialPort

data OxxiusLC = OxxiusLC {
                    olcName :: !EqName
                  , olcPort :: !SerialPort
                  , olcLasers :: ![(LSChannelName, OxxiusLaserParams)]
              }

data OxxiusLaserParams = OxxiusLaserParams {
                             oxxType :: !OxxiusLaserType
                           , oxxMaxPower :: !Double
                           , oxxWavelength :: !Int
                           , oxxRegulationMode :: !OxxiusRegulationMode
                           , oxxIndex :: !Int -- index in combiner
                           , oxxShutter :: !(Maybe Int) -- index of the optional shutter
                         }
                       deriving (Eq, Show)

data OxxiusLaserType = LBX | LCX
                      deriving (Eq, Show)

data OxxiusRegulationMode = ConstantPower | ConstantCurrent
                            deriving (Eq, Show)

initializeOxxiusLC :: EquipmentDescription -> IO EquipmentW
initializeOxxiusLC desc =
    EquipmentW <$> (initializeOxxiusLC' desc)

initializeOxxiusLC' :: EquipmentDescription -> IO OxxiusLC
initializeOxxiusLC' (OxxiusLCDesc name portName modulationMode shutterDetails) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 1000) SerialPortDebugText
    in  openSerialPort portName serialSettings >>= \port ->
        handleOxxiusCombinerCommand_ port "AS=0" >>
        handleOxxiusCombinerOKCommand port "SH1=1" >> -- open shutter 1
        (if useDigitalModulation
         then handleOxxiusCombinerEchoCommand port "AM=0" >> handleOxxiusCombinerEchoCommand port "TTL=1" -- enable digital modulation
         else handleOxxiusCombinerEchoCommand port "AM=0") >>
        getLaserDetails port shutterDetails >>= \lasers ->
        forM_ (map snd lasers) (\laserParams ->
            handleOxxiusLaserCommand port laserParams.oxxIndex "CDRH=0" >>
            oxxiusTypeSpecificInit port laserParams.oxxType laserParams.oxxIndex) >>
        pure (OxxiusLC (EqName name) port lasers)
    where
        useDigitalModulation = modulationMode == DigitalModulation
        getLaserDetails :: SerialPort -> [(Int, Int)] -> IO [(LSChannelName, OxxiusLaserParams)]
        getLaserDetails port shutterDetails = concat <$> mapM (getLaserDetail port modulationMode shutterDetails) [1 .. 6]
        getLaserDetail :: SerialPort -> WantDigitalModulation -> [(Int, Int)] -> Int -> IO [(LSChannelName, OxxiusLaserParams)]
        getLaserDetail port wantModulation shutterDetails idx =
            let msg = formatBS "L{} INF?\r\n" (T.Only idx)
            in  T.decodeUtf8 <$> serialWriteAndReadUntilChar port msg '\n' >>= \resp ->
                if (("timeout" `T.isPrefixOf` resp) || ("Not authorized" `T.isPrefixOf` resp))
                then pure []    -- no laser present at this index
                else
                    let [laserTypeStr, wavelengthStr, powerStr] = T.split (== '-') resp
                        laserType = case (laserTypeStr) of
                                       "LBX" -> LBX
                                       "LCX" -> LCX
                                       v     -> throw $ userError ("unknown oxxius lasertype " ++ T.unpack v)
                        wavelength = readT wavelengthStr
                        maxPower = readT powerStr
                        shutterIdx = lookup idx shutterDetails
                        regulationMode = if (wantModulation == DigitalModulation) then ConstantCurrent else ConstantPower
                        params = OxxiusLaserParams laserType maxPower wavelength regulationMode idx shutterIdx
                    in  pure [(LSChannelName resp, params)]
        oxxiusTypeSpecificInit :: SerialPort -> OxxiusLaserType -> Int -> IO ()
        oxxiusTypeSpecificInit port LCX idx = handleOxxiusLaserCommand_ port idx "T=0" >> -- disable temperature regulation so the laser is off until first use
        oxxiusTypeSpecificInit port LBX idx = handleOxxiusLaserCommand port idx "DL=0" >> -- turn off the laser just to make sure
                                              handleOxxiusLaserCommand_ port idx "T=1" >> -- enable temperature regulation
                                              if (useDigitalModulation)
                                              then
                                                  handleOxxiusLaserCommand port idx "ACC=1" >> -- constant current mode
                                                  handleOxxiusLaserCommand port idx "AM=0" >> -- disable analog modulation
                                                  handleOxxiusLaserCommand port idx "CW=0" >> -- disable CW
                                                  handleOxxiusLaserCommand port idx "TTL=1" -- enable digital modulation
                                              else
                                                  handleOxxiusLaserCommand port idx "TTL=0" >> -- disable digital modulation
                                                  handleOxxiusLaserCommand port idx "AM=0" >> -- disable analog modulation
                                                  handleOxxiusLaserCommand port idx "ACC=0" >> -- regulate output power mode
                                                  handleOxxiusLaserCommand port idx "CW=1" -- constant power mode

instance Equipment OxxiusLC where
    equipmentName (OxxiusLC n _ _) = n
    flushSerialPorts (OxxiusLC _ port _) = flushSerialPort port
    closeDevice (OxxiusLC _ port chs) =
        forM_ (map snd chs) (\laserParams ->
            case laserParams.oxxType of
                LBX -> handleOxxiusLaserCommand port laserParams.oxxIndex "DL=0"
                LCX -> handleOxxiusLaserCommand_ port laserParams.oxxIndex "T=0" -- turn off temperature regulation shutdown
            ) >>
        handleOxxiusCombinerOKCommand port "SH1 0" >> -- close shutter 1
        closeSerialPort port
    availableLightSources (OxxiusLC n _ chs) =
        [LightSourceDescription (LSName "ls") True True (map fst chs)]
    activateLightSource olc@(OxxiusLC _ port availChs) _ chs =
        forM_ chs (\(chName, LSIlluminationPower p) ->
            let laserParams = fromJust $ lookup chName availChs
                idx = laserParams.oxxIndex
                maybeShutterIdx = laserParams.oxxShutter
            in  handleOxxiusCombinerCommand_ port (powerCmd laserParams p) >>
                handleOxxiusLaserCommand_ port idx "T=1" >>
                handleOxxiusLaserCommand port idx "DL=1" >>
                when (isJust maybeShutterIdx) (
                    let cmd = formatT "SH{} 1" (T.Only $ fromJust maybeShutterIdx)
                    in  handleOxxiusCombinerOKCommand port cmd
                )
        )
        where
            powerCmd :: OxxiusLaserParams -> Double -> Text
            powerCmd laserParams pct = 
                case lType of
                    LCX -> formatT "PPL{} {}.{}" (idx, fst (separateParts pct), snd (separateParts pct)) -- Oxxius seems to like exactly one digit after the comma
                    LBX -> case regulationMode of
                               ConstantPower   -> formatT "PPL{} {}.{}" (idx, fst (separateParts pct), snd (separateParts pct))
                               ConstantCurrent -> formatT "L{} CM {}.{}" (idx, fst (separateParts pct), snd (separateParts pct))
                where
                    idx = laserParams.oxxIndex
                    lType = laserParams.oxxType
                    regulationMode = laserParams.oxxRegulationMode
            separateParts :: Double -> (Int, Int)
            separateParts d = (round d, round (10.0 * (d - (fromIntegral (floor d)))))
    deactivateLightSource (OxxiusLC _ port chs) =
        forM_ (map snd chs) (\laserParams ->
            if (not $ isJust laserParams.oxxShutter)
            then let cmd = case laserParams.oxxType of
                               LCX -> formatT "PPL{} 0.0" (T.Only laserParams.oxxIndex)
                               LBX -> formatT "L{} DL=0" (T.Only laserParams.oxxIndex)
                 in  handleOxxiusCombinerCommand_ port cmd
            else let cmd = formatT "SH{} 0" (T.Only $ fromJust laserParams.oxxShutter) -- close the shutter but keep the laser going
                 in  handleOxxiusCombinerOKCommand port cmd
        )

setRequestedPowers :: OxxiusLC -> [(LSChannelName, LSIlluminationPower)] -> IO ()
setRequestedPowers (OxxiusLC _ port availChs) chs =
    forM_ chs (\(chName, LSIlluminationPower p) ->
       let params = fromJust (lookup chName availChs)
       in  setLaserPower port params p)
    where
      setLaserPower :: SerialPort -> OxxiusLaserParams -> Double -> IO ()
      setLaserPower port (OxxiusLaserParams lType maxP _ _ idx _) p =
         let actualP = (p / 100.0 * maxP)
             powerStr = formatT "{}.{}" (separateParts actualP) -- Oxxius seems to like exactly one digit after the comma
         in  case lType of
                 LBX -> handleOxxiusLaserCommand port idx ("PM=" <> powerStr)
                 LCX -> handleOxxiusCombinerEchoCommand port ("P=" <> powerStr)
         where
             separateParts :: Double -> (Int, Int)
             separateParts d = (round d, round (10.0 * (d - (fromIntegral (floor d)))))

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

handleOxxiusCombinerCommand_ :: SerialPort -> Text -> IO ()
handleOxxiusCombinerCommand_ port cmd =
    T.decodeUtf8 <$> serialWriteAndReadUntilChar port (T.encodeUtf8 cmd <> "\n") '\n' >>
    pure ()

handleOxxiusLaserQuery :: SerialPort -> Int -> Text -> IO Text
handleOxxiusLaserQuery port idx q =
    let msg = formatT "L{} {}\r\n" (idx, q)
    in  handleOxxiusQuery port msg
    where
        handleOxxiusQuery :: SerialPort -> Text -> IO Text
        handleOxxiusQuery port q =
            (T.breakOn "=" . T.decodeUtf8) <$> serialWriteAndReadUntilChar port (T.encodeUtf8 q) '\n' >>= \(_, resp) ->
            pure . T.drop 1 . T.take (T.length resp - 2) $ resp

handleOxxiusLaserCommand :: SerialPort -> Int -> Text -> IO ()
handleOxxiusLaserCommand port idx comm =
    let expectedResponse = comm <> "\r\n"
        msg = formatT "L{} {}\r\n" (idx, comm)
    in  T.decodeUtf8 <$> serialWriteAndReadUntilChar port (T.encodeUtf8 msg) '\n' >>= \resp ->
        when ((resp /= expectedResponse) && (resp /= "OK\r\n")) (
            throwIO (userError ("unexpected response from Oxxius: sent " ++ T.unpack comm ++ " received " ++ T.unpack resp)))

handleOxxiusLaserCommand_ :: SerialPort -> Int -> Text -> IO ()
handleOxxiusLaserCommand_ port idx comm =
    let msg = formatT "L{} {}\r\n" (idx, comm)
    in  T.decodeUtf8 <$> serialWriteAndReadUntilChar port (T.encodeUtf8 msg) '\n' >>
        pure ()
