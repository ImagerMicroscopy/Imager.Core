{-# LANGUAGE OverloadedStrings #-}

module Equipment.Devices.Lumencor where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
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
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T
import qualified Data.Text.Format as T
import Data.Word

import Equipment.Equipment
import Equipment.EquipmentTypes
import Utils.MiscUtils
import RCSerialPort

data Lumencor = Lumencor !EqName !SerialPort !(IORef Bool) !(IORef LumencorFilter)

data MarcelLumencor = MarcelLumencor {
                          mlcName :: !EqName
                        , mlcLumencorPort :: !SerialPort
                        , mlcArduinoPort :: !SerialPort
                        , mlcFilterHasInitialization :: !(IORef Bool)
                        , mlcCurrentFilter :: !(IORef LumencorFilter)
                      }

data MLIlluminationMode = MLGated | MLTimed !LSIlluminationDuration

lumencorFromMarcelLumencor :: MarcelLumencor -> Lumencor
lumencorFromMarcelLumencor e = Lumencor (mlcName e) (mlcLumencorPort e) (mlcFilterHasInitialization e) (mlcCurrentFilter e)

initializeLumencor :: EquipmentDescription -> IO EquipmentW
initializeLumencor (LumencorLightSourceDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS9600}) (TimeoutMillis 1000) SerialPortNoDebug
        port = openSerialPort portName serialSettings
    in  EquipmentW <$> (Lumencor (EqName name) <$> port <*> newIORef False <*> newIORef LCGreenFilter)

initializeMarcelLumencor :: EquipmentDescription -> IO EquipmentW
initializeMarcelLumencor (MarcelLumencorLightSourceDesc name lcPortName arPortName) =
    let lcSerialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS9600}) (TimeoutMillis 1000) SerialPortNoDebug
        arSerialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 60000) SerialPortNoDebug
        lcPort = openSerialPort lcPortName lcSerialSettings
        arPort = openSerialPort arPortName arSerialSettings >>= \p -> threadDelay 2000000 >> pure p
    in  EquipmentW <$> (MarcelLumencor (EqName name) <$> lcPort <*> arPort <*> newIORef False <*> newIORef LCGreenFilter)

instance Equipment Lumencor where
    equipmentName (Lumencor n _ _ _) = n
    flushSerialPorts (Lumencor _ port _ _) = flushSerialPort port
    closeDevice (Lumencor _ port _ _) = closeSerialPort port
    availableLightSources (Lumencor n _ _ _) =
        [LightSourceDescription (LSName "ls") True True (map (LSChannelName . fst) lumencorChannels)]
    activateLightSource lc@(Lumencor _ port haveInitRef currFilterRef) _ chs =
        maybeChangeLumencorFilter lc lcChannels >>
        readIORef currFilterRef >>= \possiblyUpdatedFilter ->
        setLumencorChannelIntensities lc withLCChannels >>
        serialWrite port (lumencorEnableMessage lcChannels possiblyUpdatedFilter) >>
        return ()
        where
            withLCChannels = mapFirst channelNameToLCChannel chs
            lcChannels = map channelNameToLCChannel (map fst chs)
            channelNameToLCChannel = fromJust . (flip lookup lumencorChannels) . fromLSChannelName
    deactivateLightSource (Lumencor _ port _ currFilterRef) =
        readIORef currFilterRef >>= \currFilter ->
        serialWrite port (lumencorDisableMessage currFilter) >> return ()

instance Equipment MarcelLumencor where
    equipmentName e = equipmentName (lumencorFromMarcelLumencor e)
    flushSerialPorts (MarcelLumencor _ lcP arP _ _) = flushSerialPort lcP >> flushSerialPort arP
    closeDevice (MarcelLumencor _ lcP arP _ _) = closeSerialPort lcP >> closeSerialPort arP
    availableLightSources e = availableLightSources (lumencorFromMarcelLumencor e)
    activateLightSource ml _ chs = handleMLIllumination ml chs MLGated
    activateLightSourceTimed ml _ chs dur = handleMLIllumination ml chs (MLTimed dur)
    deactivateLightSource ml =
        handleMarcelLumencorMessage ml "x" >>
        deactivateLightSource (lumencorFromMarcelLumencor ml)

handleMLIllumination :: MarcelLumencor -> [(LSChannelName, LSIlluminationPower)] -> MLIlluminationMode -> IO ()
handleMLIllumination ml chs mode =
    maybeChangeLumencorFilter (lumencorFromMarcelLumencor ml) (map fst withLCChannels) >>
    setLumencorChannelIntensities (lumencorFromMarcelLumencor ml) withLCChannels >>
    handleMarcelLumencorMessage ml theMsg
    where
        withLCChannels = mapFirst channelNameToLCChannel chs
        channelNameToLCChannel = fromJust . (flip lookup lumencorChannels) . fromLSChannelName
        theMsg = case mode of
                     MLGated     -> prepareMsg
                     MLTimed dur -> let durationUS = max (10 :: Int) (round (1.0e6 * fromLSIlluminationDuration dur))
                                    in  T.encodeUtf8 $ LT.toStrict $ T.format "irr:{}:0:i {}" (durationUS, T.decodeUtf8 prepareMsg)
        prepareMsg = B.intercalate " " (map singleChannelMsg (map fst withLCChannels))
        singleChannelMsg lch = let chCode = fromJust $ lookup lch marcelLumencorChannelCoding
                                   nRepeats = case mode of
                                                  MLGated   -> "65535"
                                                  MLTimed _ -> "1"
                               in  chCode <> nRepeats

data LumencorChannel = LCViolet | LCBlue | LCCyan |LCTeal
                     | LCGreen | LCYellow | LCRed
                       deriving (Eq)
data LumencorFilter = LCGreenFilter | LCYellowFilter
                      deriving (Eq)

lumencorChannels :: [(Text, LumencorChannel)]
lumencorChannels = [("violet", LCViolet), ("blue", LCBlue), ("cyan", LCCyan), ("teal", LCTeal),
                    ("green", LCGreen), ("yellow", LCYellow), ("red", LCRed)]

marcelLumencorChannelCoding :: [(LumencorChannel, ByteString)]
marcelLumencorChannelCoding = [(LCViolet, "V"), (LCBlue, "B"), (LCCyan, "C"),
                               (LCTeal, "T"), (LCGreen, "G"), (LCYellow, "Y"),
                               (LCRed, "R")]

maybeChangeLumencorFilter :: Lumencor -> [LumencorChannel] -> IO ()
maybeChangeLumencorFilter (Lumencor _ port haveInitRef currFilterRef) lcChannels =
   readIORef haveInitRef >>= \haveInit ->
   when (not haveInit) (   -- init RS232 and arbitrarily select the green filter
       serialWrite port lumencorEnableRS232Message >>
       changeFilter LCGreenFilter >>
       writeIORef haveInitRef True) >>
   readIORef currFilterRef >>= \currFilter ->
   when ((isJust filterForChannel) && (currFilter /= fromJust filterForChannel)) (
        changeFilter $ fromJust filterForChannel)
    where
       filterForChannel | LCGreen `elem` lcChannels  = Just LCGreenFilter
                        | LCYellow `elem` lcChannels = Just LCYellowFilter
                        | otherwise                  = Nothing
       changeFilter filter =
         serialWrite port (lumencorFilterMessage filter) >>
         threadDelay (floor (0.3 * 1.0e6)) >>
         writeIORef currFilterRef filter

setLumencorChannelIntensities :: Lumencor -> [(LumencorChannel, LSIlluminationPower)] -> IO ()
setLumencorChannelIntensities (Lumencor _ port _ _) lcs =
    serialWrite port (lumencorIntensityMessage channels powers)
    where
        (channels, powers) = unzip lcs

lumencorEnableRS232Message :: ByteString
lumencorEnableRS232Message =
    B.pack [0x57, 0x02, 0xFF, 0x50, 0x57, 0x03, 0xAB, 0x50]

lumencorFilterMessage :: LumencorFilter -> ByteString
lumencorFilterMessage = lumencorDisableMessage

lumencorEnableMessage :: [LumencorChannel] -> LumencorFilter -> ByteString
lumencorEnableMessage channels filter =
    B.pack [0x4F, enableByte, 0x50]
    where
        enableByte :: Word8
        enableByte = let channelsEnable = 0x7F .&. (complement $  foldl' (\accum ch -> accum .|. (channelEnableByte ch)) 0 channels)
                     in if (filter == LCGreenFilter)
                        then channelsEnable .|. 0x10
                        else channelsEnable .&. complement 0x10
        channelEnableByte LCViolet = 2^3
        channelEnableByte LCBlue = 2^5
        channelEnableByte LCCyan = 2^2
        channelEnableByte LCTeal = 2^6
        channelEnableByte LCYellow = 2^1
        channelEnableByte LCGreen = 2^1
        channelEnableByte LCRed = 2^0

lumencorIntensityMessage :: [LumencorChannel] -> [LSIlluminationPower] -> ByteString
lumencorIntensityMessage chs ps = mconcat (zipWith lumencorChannelIntensityMessage chs ps)

lumencorChannelIntensityMessage :: LumencorChannel -> LSIlluminationPower -> ByteString
lumencorChannelIntensityMessage ch (LSIlluminationPower p) =
    B.pack [0x53, byte5, 0x03, byte3, byte2, byte1, 0x50]
    where
        intensityByte = round ((1.0 - p / 100.0) * 255) -- 255 means no light
        dacAndIntensity LCViolet = (0x18, 2^0)
        dacAndIntensity LCBlue = (0x1A, 2^0)
        dacAndIntensity LCCyan = (0x18, 2^1)
        dacAndIntensity LCTeal = (0x1A, 2^1)
        dacAndIntensity LCGreen = (0x18, 2^2)
        dacAndIntensity LCYellow = (0x18, 2^2)
        dacAndIntensity LCRed = (0x18, 2^3)
        byte5 = fst (dacAndIntensity ch)
        byte3 = snd (dacAndIntensity ch)
        byte2 = (intensityByte `shiftR` 4) .|. 0xF0
        byte1 = intensityByte `shiftL` 4

lumencorDisableMessage :: LumencorFilter -> ByteString
lumencorDisableMessage filter =
    B.pack [0x4F, filterSelectByte filter, 0x50]
    where
        filterSelectByte LCGreenFilter = 0x7F
        filterSelectByte LCYellowFilter = 0x6F

handleMarcelLumencorMessage :: MarcelLumencor -> ByteString -> IO ()
handleMarcelLumencorMessage (MarcelLumencor _ _ arPort _ _) bs =
    serialWriteAndReadUntilChar arPort (bs <> "\r\n") '\n' >>= \resp ->
    when (B.take 2 resp /= "OK") (throwIO $ userError "Error response from MarcelLumencor Arduino")
