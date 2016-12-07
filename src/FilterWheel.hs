{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module FilterWheel where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Either
import Data.IORef
import Data.List
import Data.Maybe
import Data.Serialize hiding(flush)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment
import System.FilePath
import System.Hardware.Serialport hiding(timeout)
import System.Timeout

import MiscUtils

data FilterWheelDesc = ThorlabsFW103HDesc {
                           fw103DescName :: !Text
                         , fw103DescPortName :: !String
                         , fw103DescFilters :: [(Text, Int)]
                       }
                     | ThorlabsFW102CDesc {
                           fw102CDescName :: !Text
                         , fw102CDescPortName :: !String
                         , fw102CDescFilters :: [(Text, Int)]
                       }
                     | DummyFilterWheelDesc {
                           dfwDescName :: !Text
                         , dfwDescFilters :: [(Text, Int)]
                       }
                     deriving (Read, Show)

data FilterWheel = ThorlabsFW103H !Text ![(Text, Int)] !SerialPort
                 | ThorlabsFW102C !Text ![(Text, Int)] !SerialPort
                 | DummyFilterWheel !Text ![(Text, Int)]

instance ToJSON FilterWheel where
  toJSON fw = object ["name" .= filterWheelName fw, "channels" .= filterWheelChannels fw]

readAvailableFilterWheels :: IO [FilterWheelDesc]
readAvailableFilterWheels =
  getExecutablePath >>= \exePath ->
  readFile (takeDirectory exePath </> confFilename) >>=
  return . read
  where
    confFilename = "filterwheels.txt"

withFilterWheels :: [FilterWheelDesc] -> ([FilterWheel] -> IO a) -> IO a
withFilterWheels descs action =
    bracket (openFilterWheels descs) closeFilterWheels action

filterWheelName :: FilterWheel -> Text
filterWheelName (ThorlabsFW103H name _ _) = name
filterWheelName (ThorlabsFW102C name _ _) = name
filterWheelName (DummyFilterWheel name _) = name

filterWheelChannels :: FilterWheel -> [Text]
filterWheelChannels (ThorlabsFW103H _ chs _) = map fst chs
filterWheelChannels (ThorlabsFW102C _ chs _) = map fst chs
filterWheelChannels (DummyFilterWheel _ chs) = map fst chs

openFilterWheels :: [FilterWheelDesc] -> IO [FilterWheel]
openFilterWheels = mapM openFilterWheel
  where
    openFilterWheel (ThorlabsFW103HDesc name portName chs) =
        openSerialWithErrorMsg portName (defaultSerialSettings {commSpeed = CS115200}) >>= \port ->
        forM_ fw103HStartupMessages (\msg -> debugSend port msg >> threadDelay (floor 50e3)) >>
        debugSend port fw103HStopUpdatesMessage >>
        return (ThorlabsFW103H name (validateChannels chs) port)
    openFilterWheel (ThorlabsFW102CDesc name portName chs) =
        ThorlabsFW102C name (validateChannels chs) <$> openSerialWithErrorMsg portName (defaultSerialSettings {commSpeed = CS115200})
    openFilterWheel (DummyFilterWheelDesc name chs) =
        putStrLn ("Opened dummy filter wheel " ++ T.unpack name ++ " with filters " ++ show chs) >> return (DummyFilterWheel name (validateChannels chs))
    validateChannels :: [(Text, Int)] -> [(Text, Int)]
    validateChannels chs | haveDuplicates chs = error ("duplicate channels in " ++ show chs)
                         | invalidFilterIndices chs = error ("invalid filter indices in "  ++ show chs)
                         | invalidFilterNames chs = error ("invalid filter names in " ++ show chs)
                         | otherwise = chs
      where
        haveDuplicates chs = not ((nodups (map fst chs)) && (nodups (map snd chs)))
        nodups xs = (nub xs) == xs
        invalidFilterIndices = any (\(_, i) -> not (within i 0 5))
        invalidFilterNames = any (\(n, _) -> T.null n)

closeFilterWheels :: [FilterWheel] -> IO ()
closeFilterWheels = mapM_ closeFilterWheels
  where
    closeFilterWheels (ThorlabsFW103H _ _ port) = closeSerial port
    closeFilterWheels (ThorlabsFW102C _ _ port) = closeSerial port
    closeFilterWheels (DummyFilterWheel name _) = putStrLn ("Closed filter wheel " ++ T.unpack name)

filterWheelHasChannel :: FilterWheel -> Text -> Bool
filterWheelHasChannel (ThorlabsFW103H _ chs _) c = c `elem` (map fst chs)
filterWheelHasChannel (ThorlabsFW102C _ chs _) c = c `elem` (map fst chs)
filterWheelHasChannel (DummyFilterWheel _ chs) c = c `elem` (map fst chs)

switchFilterWheel :: [FilterWheel] -> Text -> Text -> IO (Either String ())
switchFilterWheel fws fwName fName =
    let filterWheel = head (filter (\fw -> filterWheelName fw == fwName) fws)
    in timeout (floor 2.0e6) (switchToFilter filterWheel fName) >>= \result ->
       case result of
           Nothing -> return (Left ("timeout communicating with " ++ T.unpack (filterWheelName filterWheel)))
           Just v -> return v

switchToFilter :: FilterWheel -> Text -> IO (Either String ())
switchToFilter fw chName | not (filterWheelHasChannel fw chName) = return (Left "no matching channel for filter wheel")
                         | otherwise = switchToFilter' fw chName
  where
    switchToFilter' :: FilterWheel -> Text -> IO (Either String ())
    switchToFilter' (ThorlabsFW103H _ chs port) chName =
        let filterIndex = fromJust (lookup chName chs)
            wheelPos = (409600 `div` 6) * filterIndex -- Thorlabs:  1 turn represents 360 degrees which is 409600 micro steps
        in  debugSend port (fw103HMoveAbsoluteMessage wheelPos) >> --threadDelay (floor 150e3) >> return (Right ())
            timeout (floor 1e6) (fw103HWaitUntilMotionStops port) >>= \result ->
            case result of
                Nothing -> return (Left "no reply from filter wheel")
                Just () -> return (Right ())
    switchToFilter' (ThorlabsFW102C _ chs port) chName =
        let filterIndex = fromJust (lookup chName chs)
        in flush port >> send port (T.encodeUtf8 . T.pack $ "pos=" ++ show filterIndex) >>
           readFromSerialUntilChar port '>' >> return (Right ())
    switchToFilter' (DummyFilterWheel name chs) chName =
        putStrLn ("Switched filter wheel " ++ T.unpack name ++ " to filter " ++ T.unpack chName) >> return (Right ())

fw103HMoveAbsoluteMessage :: Int -> ByteString
fw103HMoveAbsoluteMessage pos = runPut $
    mapM_ putWord8 [0x53, 0x04, 0x06, 0x00, 0xD0, 0x01, 0x01, 0x00] >>
    putWord32le (fromIntegral pos)

fw103HWaitUntilMotionStops :: SerialPort -> IO ()
fw103HWaitUntilMotionStops port =
    flush port >> send port fw103HGetStatusMessage >>
    readAtLeastNBytesFromSerial port 20 >>= \response ->
    putStrLn ("received " ++ byteStringAsHex response) >>
    case (0xF0 .&. (B.index response 16)) of
        0 -> return ()
        _ -> fw103HWaitUntilMotionStops port

fw103HInitializationMessage :: ByteString
fw103HInitializationMessage = B.pack [0x18, 0x00, 0x00, 0x00, 0x50, 0x01]

fw103HEnableChannelMessage :: ByteString
fw103HEnableChannelMessage = B.pack [0x10, 0x02, 0x01, 0x01, 0x50, 0x01]

fw103HStopUpdatesMessage :: ByteString
fw103HStopUpdatesMessage = B.pack [0x12, 0x00, 0x00, 0x00, 0x50, 0x01]

fw103HMoveHomeMessage :: ByteString
fw103HMoveHomeMessage = B.pack [0x43, 0x04, 0x01, 0x00, 0x50, 0x01]

fw103HGetStatusMessage :: ByteString
fw103HGetStatusMessage = B.pack [0x80, 0x04, 0x01, 0x00, 0x50, 0x01]

fw103HStartupMessages :: [ByteString] -- copied from the APT software log
fw103HStartupMessages =
    map B.pack  [[0x18, 0x00, 0x00, 0x00, 0x50, 0x01],[0x05, 0x00, 0x00, 0x00, 0x50, 0x01],
                 [0x10, 0x02, 0x01, 0x01, 0x50, 0x01],[0x13, 0x02, 0x00, 0x00, 0x50, 0x01],
                 [0x00, 0x05, 0x01, 0x10, 0x50, 0x01],[0x13, 0x04, 0x0E, 0x00, 0xD0, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x5F, 0x45, 0x0F, 0x00, 0xC0, 0x9B, 0x35, 0x1A],
                 [0x16, 0x04, 0x16, 0x00, 0xD0, 0x01, 0x01, 0x00, 0x02, 0x00, 0xAB, 0x0A, 0x01, 0x00, 0xF7, 0x14, 0x00, 0x00, 0x5F, 0x45, 0x0F, 0x00, 0xC0, 0x9B, 0x35, 0x1A, 0x02, 0x00],
                 [0x23, 0x04, 0x10, 0x00, 0xD0, 0x01, 0x01, 0x00, 0x02, 0x00, 0x02, 0x00, 0x55, 0x0D, 0x00, 0x00, 0x72, 0x04, 0x00, 0x00, 0x41, 0x00],
                 [0x26, 0x04, 0x06, 0x00, 0xD0, 0x01, 0x01, 0x00, 0x14, 0x00, 0x64, 0x00],
                 [0x3A, 0x04, 0x06, 0x00, 0xD0, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00],
                 [0x40, 0x04, 0x0E, 0x00, 0xD0, 0x01, 0x01, 0x00, 0x02, 0x00, 0x01, 0x00, 0x6E, 0x30, 0x5D, 0x00, 0x40, 0xDE, 0x00, 0x00],
                 [0x45, 0x04, 0x06, 0x00, 0xD0, 0x01, 0x01, 0x00, 0x00, 0x0A, 0x00, 0x00],
                 [0x50, 0x04, 0x06, 0x00, 0xD0, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00],
                 [0xF4, 0x04, 0x04, 0x00, 0xD0, 0x01, 0x01, 0x00, 0x00, 0x00],
                 [0xE6, 0x04, 0x14, 0x00, 0xD0, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x80, 0x50, 0xB6, 0x97, 0xFE, 0x50, 0xB6, 0x97, 0xFE, 0x02, 0x00],
                 [0x10, 0x02, 0x02, 0x01, 0x50, 0x01],[0x13, 0x02, 0x00, 0x00, 0x50, 0x01],
                 [0x00, 0x05, 0x02, 0x10, 0x50, 0x01],
                 [0x13, 0x04, 0x0E, 0x00, 0xD0, 0x01, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x83, 0x7C, 0x00, 0x00, 0x00, 0xF4, 0x01, 0x00],
                 [0x16, 0x04, 0x16, 0x00, 0xD0, 0x01, 0x02, 0x00, 0x02, 0x00, 0x00, 0x7D, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x83, 0x7C, 0x00, 0x00, 0x00, 0x7D, 0x00, 0x00, 0x00, 0x00],
                 [0x23, 0x04, 0x10, 0x00, 0xD0, 0x01, 0x02, 0x00, 0x03, 0x00, 0x01, 0x00, 0x9D, 0xFF, 0xFF, 0xFF, 0x9D, 0xFF, 0xFF, 0xFF, 0x01, 0x00],
                 [0x26, 0x04, 0x06, 0x00, 0xD0, 0x01, 0x02, 0x00, 0x14, 0x00, 0x64, 0x00],
                 [0x3A, 0x04, 0x06, 0x00, 0xD0, 0x01, 0x02, 0x00, 0x00, 0x0A, 0x00, 0x00],
                 [0x40, 0x04, 0x0E, 0x00, 0xD0, 0x01, 0x02, 0x00, 0x02, 0x00, 0x01, 0x00, 0x00, 0x7D, 0x00, 0x00, 0x00, 0x32, 0x00, 0x00],
                 [0x45, 0x04, 0x06, 0x00, 0xD0, 0x01, 0x02, 0x00, 0x00, 0x0A, 0x00, 0x00],
                 [0x50, 0x04, 0x06, 0x00, 0xD0, 0x01, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00],
                 [0xF4, 0x04, 0x04, 0x00, 0xD0, 0x01, 0x02, 0x00, 0x00, 0x00],
                 [0xE6, 0x04, 0x14, 0x00, 0xD0, 0x01, 0x02, 0x00, 0x4A, 0x0C, 0x02, 0x00, 0xE1, 0x7A, 0x14, 0x00, 0x13, 0x01, 0x00, 0x00, 0x13, 0x01, 0x00, 0x00, 0x01, 0x00],
                 [0x11, 0x00, 0x0A, 0x01, 0x50, 0x01]]
