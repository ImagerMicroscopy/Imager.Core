module Equipment.Devices.BlueBoxOptics where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Format as LT
import qualified Data.Text.Lazy as LT

import Equipment.Equipment
import Equipment.EquipmentTypes
import RCSerialPort

data BlueBoxNiji = BlueBoxNiji {
                       bbnEqName :: !EqName
                     , bbnPort :: !SerialPort
                   }

data BlueBoxNijiChannelNames = BBNUltraViolet
                             | BBNRoyalBlue
                             | BBNBlue
                             | BBNCyan
                             | BBNGreenAmber
                             | BBNRed
                             | BBNFarRed
                             deriving (Eq, Show)

initializeBlueBoxNiji :: EquipmentDescription -> IO EquipmentW
initializeBlueBoxNiji (BlueBoxNijiDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 1000) SerialPortNoDebug
        port = openSerialPort portName serialSettings
    in  EquipmentW <$> (BlueBoxNiji (EqName name) <$> port)

instance Equipment BlueBoxNiji where
    equipmentName = bbnEqName
    flushSerialPorts = flushSerialPort . bbnPort
    closeDevice = closeSerialPort . bbnPort
    availableLightSources _ =
        [LightSourceDescription (LSName "ls") True True (map (LSChannelName . fst) blueBoxChannelsNamesAndTypes)]
    activateLightSource (BlueBoxNiji _ port) _ chs =
        setAllPowers >> activateChannels
        where
            requestedChannels = map (\chName -> fromJust $ lookup chName blueBoxChannelsNamesAndTypes) (map (fromLSChannelName . fst) chs)
            requestedPowers = map (fromLSIlluminationPower . snd) chs
            setAllPowers = forM_ (zip requestedChannels requestedPowers) (\(ch, p) ->
                               let cmdStr = LT.format "d,{},{}" (blueBoxChannelIndex ch, (round p :: Int))
                               in  sendBlueBoxNijiMessage port (LT.toStrict cmdStr))
            activateChannels = forM_ requestedChannels (\ch ->
                                   let cmdStr = LT.format "D,{},1" (LT.Only (blueBoxChannelIndex ch))
                                   in  sendBlueBoxNijiMessage port (LT.toStrict cmdStr))
    deactivateLightSource (BlueBoxNiji _ port) =
        forM_ [1 .. 7 :: Int] (\i ->
            let cmdStr = LT.format "D,{},0" (LT.Only i)
            in  sendBlueBoxNijiMessage port (LT.toStrict cmdStr))

blueBoxChannelsNamesAndTypes :: [(Text, BlueBoxNijiChannelNames)]
blueBoxChannelsNamesAndTypes =
    [("UV", BBNUltraViolet), ("Royal Blue", BBNRoyalBlue), ("Blue", BBNBlue),
     ("Cyan", BBNCyan), ("GreenAmber", BBNGreenAmber), ("Red", BBNRed),
     ("Far Red", BBNFarRed)]

blueBoxChannelIndex :: BlueBoxNijiChannelNames -> Int
blueBoxChannelIndex ch = fromJust $ lookup ch (zip channels [1 .. 7])
    where
        channels = map snd blueBoxChannelsNamesAndTypes

sendBlueBoxNijiMessage :: SerialPort -> Text -> IO ()
sendBlueBoxNijiMessage port ts = serialWrite port (T.encodeUtf8 ts <> "\r\n")
