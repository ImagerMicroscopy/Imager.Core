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
                           , oxxIndex :: !Int -- index in combiner
                           , oxxShutter :: !(Maybe Int) -- index of the optional shutter
                         }
                       deriving (Eq, Show)

data OxxiusLaserType = LBX | LCX
                      deriving (Eq, Show)

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
         then handleOxxiusCombinerEchoCommand port "AM=1" -- enable analog modulation
         else handleOxxiusCombinerEchoCommand port "AM=0") >>
        getLaserDetails port shutterDetails >>= \lasers ->
        forM_ (map snd lasers) (\laserParams ->
            handleOxxiusLaserCommand port laserParams.oxxIndex "CDRH=0" >>
            oxxiusTypeSpecificInit port laserParams.oxxType laserParams.oxxIndex) >>
        pure (OxxiusLC (EqName name) port lasers)
    where
        useDigitalModulation = modulationMode == DigitalModulation
        getLaserDetails :: SerialPort -> [(Int, Int)] -> IO [(LSChannelName, OxxiusLaserParams)]
        getLaserDetails port shutterDetails = concat <$> mapM (getLaserDetail port shutterDetails) [1 .. 6]
        getLaserDetail :: SerialPort -> [(Int, Int)] -> Int -> IO [(LSChannelName, OxxiusLaserParams)]
        getLaserDetail port shutterDetails idx =
            let msg = T.encodeUtf8 . LT.toStrict $ T.format "L{} INF?\r\n" (T.Only idx)
            in  T.decodeUtf8 <$> serialWriteAndReadUntilChar port msg '\n' >>= \resp ->
                if (("timeout" `T.isPrefixOf` resp) || ("Not authorized" `T.isPrefixOf` resp))
                then pure []    -- no laser present at this index
                else
                    let [laserTypeStr, wavelengthStr, powerStr] = T.split (== '-') resp
                        laserType = case (laserTypeStr) of
                                       "LBX" -> LBX
                                       "LCX" -> LCX
                                       v     -> throw $ userError ("unknown oxxius lasertype " ++ T.unpack v)
                        wavelength = read (T.unpack wavelengthStr)
                        maxPower = read (T.unpack powerStr)
                        shutterIdx = lookup idx shutterDetails
                        params = OxxiusLaserParams laserType maxPower wavelength idx shutterIdx
                    in  pure [(LSChannelName resp, params)]
        oxxiusTypeSpecificInit :: SerialPort -> OxxiusLaserType -> Int -> IO ()
        oxxiusTypeSpecificInit port LCX idx = handleOxxiusLaserCommand_ port idx "T=0" -- disable temperature regulation so the laser is off until first use
        oxxiusTypeSpecificInit port LBX idx = handleOxxiusLaserCommand port idx "L=0" >> -- turn off the laser just to make sure
                                              handleOxxiusLaserCommand_ port idx "T=1" >> -- enable temperature regulation
                                              if (useDigitalModulation)
                                              then
                                                  handleOxxiusLaserCommand port idx "ACC=1" >> -- constant current mode  
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
                LBX -> handleOxxiusLaserCommand port laserParams.oxxIndex "L=0"
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
            in  handleOxxiusCombinerCommand_ port (powerCmd idx p) >>
                handleOxxiusLaserCommand_ port idx "T=1" >>
                handleOxxiusLaserCommand port idx "L=1" >>
                when (isJust maybeShutterIdx) (
                    let cmd = LT.toStrict (T.format "SH{} 1" (T.Only $ fromJust maybeShutterIdx))
                    in  handleOxxiusCombinerOKCommand port cmd
                )
        )
        where
            powerCmd :: Int -> Double -> Text
            powerCmd idx pct = LT.toStrict (T.format "PPL{} {}.{}" (idx, fst (separateParts pct), snd (separateParts pct))) -- Oxxius seems to like exactly one digit after the comma
            separateParts :: Double -> (Int, Int)
            separateParts d = (round d, round (10.0 * (d - (fromIntegral (floor d)))))
    deactivateLightSource (OxxiusLC _ port chs) =
        forM_ (map snd chs) (\laserParams ->
            if (not $ isJust laserParams.oxxShutter)
            then let cmd = LT.toStrict (T.format "PPL{} 0.0" (T.Only laserParams.oxxIndex))
                 in  handleOxxiusCombinerCommand_ port cmd
            else let cmd = LT.toStrict (T.format "SH{} 0" (T.Only $ fromJust laserParams.oxxShutter)) -- close the shutter but keep the laser going
                 in  handleOxxiusCombinerOKCommand port cmd
        )

initializeMarcelOxxiusLC :: EquipmentDescription -> IO EquipmentW
initializeMarcelOxxiusLC (MarcelOxxiusLCDesc name oxxPortName ardPortName) =
    let arSerialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 60000) SerialPortNoDebug
    in  initializeOxxiusLC' (OxxiusLCDesc name oxxPortName DigitalModulation []) >>= \(ox@(OxxiusLC _ oxPort lasers)) ->
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

setRequestedPowers :: OxxiusLC -> [(LSChannelName, LSIlluminationPower)] -> IO ()
setRequestedPowers (OxxiusLC _ port availChs) chs =
    forM_ chs (\(chName, LSIlluminationPower p) ->
       let params = fromJust (lookup chName availChs)
       in  setLaserPower port params p)
    where
      setLaserPower :: SerialPort -> OxxiusLaserParams -> Double -> IO ()
      setLaserPower port (OxxiusLaserParams lType maxP _ idx _) p =
         let actualP = (p / 100.0 * maxP)
             powerStr = LT.toStrict (T.format "{}.{}" (separateParts actualP)) -- Oxxius seems to like exactly one digit after the comma
         in  case lType of
                 LBX -> handleOxxiusLaserCommand port idx ("PM=" <> powerStr)
                 LCX -> handleOxxiusCombinerEchoCommand port ("P=" <> powerStr)
         where
             separateParts :: Double -> (Int, Int)
             separateParts d = (round d, round (10.0 * (d - (fromIntegral (floor d)))))

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
        marcelOxxiusCodeForLaser (OxxiusLaserParams _ _ wl _ _)
            | within wl 380 420 = "V"
            | within wl 420 470 = "B"
            | within wl 470 520 = "C"
            | within wl 520 600 = "G"
            | within wl 600 660 = "R"
            | otherwise         = error ("No oxxius code for wavelength " ++ show wl)
            where
                within :: (Ord a) => a -> a -> a -> Bool
                within a b c = (a >= b) && (a <= c)

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
        when ((resp /= expectedResponse) && (resp /= "OK\r\n")) (
            throwIO (userError ("unexpected response from Oxxius: sent " ++ T.unpack comm ++ " received " ++ T.unpack resp)))

handleOxxiusLaserCommand_ :: SerialPort -> Int -> Text -> IO ()
handleOxxiusLaserCommand_ port idx comm =
    let msg = LT.toStrict (T.format "L{} {}\r\n" (idx, comm))
    in  T.decodeUtf8 <$> serialWriteAndReadUntilChar port (T.encodeUtf8 msg) '\n' >>
        pure ()

handleMarcelOxxiusMessage :: MarcelOxxiusLC -> ByteString -> IO ()
handleMarcelOxxiusMessage (MarcelOxxiusLC _ arPort) bs =
    serialWriteAndReadUntilChar arPort (bs <> "\r\n") '\n' >>= \resp ->
    when (B.take 2 resp /= "OK") (throwIO $ userError "Error response from MarcelOxxius Arduino")
