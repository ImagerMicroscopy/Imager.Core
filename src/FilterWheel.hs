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
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment
import System.FilePath
import qualified System.Timeout as ST

import MiscUtils
import RCSerialPort

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
                     | OlympusIX71DichroicDesc {
                           ix71DescName :: !Text
                         , ix71DescPortName :: !String
                         , ix71DescFilters :: ![(Text, Int)]
                       }
                     | SutterLambda10BDesc {
                           sl10BDescName :: !Text
                         , sl10BDescPortName :: !String
                         , sl10BDescFilters :: [(Text, Int)]
                       }
                     | DummyFilterWheelDesc {
                           dfwDescName :: !Text
                         , dfwDescFilters :: [(Text, Int)]
                       }
                     deriving (Read, Show)

data FilterWheel = ThorlabsFW103H !Text ![(Text, Int)] !SerialPort
                 | ThorlabsFW102C !Text ![(Text, Int)] !SerialPort
                 | SutterLambda10B !Text ![(Text, Int)] !SerialPort
                 | OlympusIX71Dichroic !Text ![(Text, Int)] !(IORef (Bool, Int)) !SerialPort
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
filterWheelName (SutterLambda10B name _ _ ) = name
filterWheelName (OlympusIX71Dichroic name _ _ _) = name
filterWheelName (DummyFilterWheel name _) = name

filterWheelChannels :: FilterWheel -> [Text]
filterWheelChannels (ThorlabsFW103H _ chs _) = map fst chs
filterWheelChannels (ThorlabsFW102C _ chs _) = map fst chs
filterWheelChannels (SutterLambda10B _ chs _) = map fst chs
filterWheelChannels (OlympusIX71Dichroic _ chs _ _) = map fst chs
filterWheelChannels (DummyFilterWheel _ chs) = map fst chs

openFilterWheels :: [FilterWheelDesc] -> IO [FilterWheel]
openFilterWheels = mapM openFilterWheel
  where
    openFilterWheel (ThorlabsFW103HDesc name portName chs) =
        let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 10000) SerialPortNoDebug
        in  openSerialPort portName serialSettings >>= \port ->
            forM_ fw103HStartupMessages (\msg -> serialWrite port msg >> threadDelay (floor 25e3)) >>
            serialWrite port fw103HStopUpdatesMessage >> serialWrite port fw103HMoveHomeMessage >>
            fw103HWaitUntilHomingStops port >>
            return (ThorlabsFW103H name (validateChannels (0, 5) chs) port)
    openFilterWheel (ThorlabsFW102CDesc name portName chs) =
        let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 30000) SerialPortNoDebug
        in  ThorlabsFW102C name (validateChannels (0, 5) chs) <$> openSerialPort portName serialSettings
    openFilterWheel (SutterLambda10BDesc name portName chs) =
        let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS128000}) (TimeoutMillis 10000) SerialPortDebugBinary
        in  openSerialPort portName serialSettings >>= \port ->
            serialWriteByte port 238 >> serialReadUntilChar port '\r'>>
            serialWriteByte port 253 >> serialReadUntilChar port '\r' >>
            return (SutterLambda10B name (validateChannels (0, 9) chs) port)
    openFilterWheel (OlympusIX71DichroicDesc name portName chs) =
        let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS19200}) (TimeoutMillis 20000) SerialPortNoDebug
        in  openSerialPort portName serialSettings >>= \port ->
            putStr "initializing IX71 motorized dichroic..." >>
            serialWrite port "1LOG IN\r" >> serialReadUntilChar port '\r' >>= \response ->
            when (response /= "1LOG +\r") (
                putStrLn ("unexpected response from Olymus IX71 DM: " ++ show response) >> putStrLn "press return to close" >> getLine >> error "failed") >>
            putStrLn "done!" >> newIORef (False, 0) >>= \currFilterRef ->
            return (OlympusIX71Dichroic name (validateChannels (0, 5) chs) currFilterRef port)
    openFilterWheel (DummyFilterWheelDesc name chs) =
        putStrLn ("Opened dummy filter wheel " ++ T.unpack name ++ " with filters " ++ show chs) >> return (DummyFilterWheel name (validateChannels (0, 5) chs))
    validateChannels :: (Int, Int) -> [(Text, Int)] -> [(Text, Int)]
    validateChannels idxLimits chs
        | haveDuplicates chs = error ("duplicate channels in " ++ show chs)
        | invalidFilterIndices idxLimits chs = error ("invalid filter indices in "  ++ show chs)
        | invalidFilterNames chs = error ("invalid filter names in " ++ show chs)
        | otherwise = chs
      where
        haveDuplicates chs = not ((nodups (map fst chs)) && (nodups (map snd chs)))
        nodups xs = (nub xs) == xs
        invalidFilterIndices (minIdx, maxIdx) = any (\(_, i) -> not (within i minIdx maxIdx))
        invalidFilterNames = any (\(n, _) -> T.null n)

closeFilterWheels :: [FilterWheel] -> IO ()
closeFilterWheels = mapM_ closeFilterWheels
  where
    closeFilterWheels (ThorlabsFW103H _ _ port) = closeSerialPort port
    closeFilterWheels (ThorlabsFW102C _ _ port) = closeSerialPort port
    closeFilterWheels (SutterLambda10B _ _ port) = closeSerialPort port
    closeFilterWheels (OlympusIX71Dichroic _ _ _ port) = closeSerialPort port
    closeFilterWheels (DummyFilterWheel name _) = putStrLn ("Closed filter wheel " ++ T.unpack name)

filterWheelHasChannel :: FilterWheel -> Text -> Bool
filterWheelHasChannel w c = c `elem` (filterWheelChannels w)

switchFilterWheel :: [FilterWheel] -> Text -> Text -> IO ()
switchFilterWheel fws fwName fName =
    let filterWheel = head (filter (\fw -> filterWheelName fw == fwName) fws)
    in ST.timeout (floor 10e6) (switchToFilter filterWheel fName) >>= \result ->
       case result of
           Nothing -> throwIO (userError ("timeout communicating with " ++ T.unpack (filterWheelName filterWheel)))
           Just v -> return v

switchToFilter :: FilterWheel -> Text -> IO ()
switchToFilter fw chName | not (filterWheelHasChannel fw chName) = throwIO (userError "no matching channel for filter wheel")
                         | otherwise = switchToFilter' fw chName
  where
    switchToFilter' :: FilterWheel -> Text -> IO ()
    switchToFilter' (ThorlabsFW103H _ chs port) chName =
        let filterIndex = fromJust (lookup chName chs)
            wheelPos = (409600 `div` 6) * filterIndex -- Thorlabs:  1 turn represents 360 degrees which is 409600 micro steps
        in  flushSerialPort port >> serialWrite port (fw103HMoveAbsoluteMessage wheelPos) >>
            fw103HWaitUntilMotionStops port wheelPos
    switchToFilter' (ThorlabsFW102C _ chs port) chName =
        let filterIndex = fromJust (lookup chName chs)
        in flushSerialPort port >> serialWrite port (T.encodeUtf8 . T.pack $ "pos=" ++ show filterIndex) >>
           serialReadUntilChar port '>' >> return ()
    switchToFilter' (SutterLambda10B _ chs port) chName =
        let filterIndex = (fromIntegral . fromJust . lookup chName) chs
            speed = 3
            byte = (speed `shiftL` 4) .|. filterIndex
        in flushSerialPort port >> serialWrite port (B.pack [byte]) >>
          serialReadUntilChar port '\r' >> return ()
    switchToFilter' (OlympusIX71Dichroic _ chs currFilter port) chName =
        let filterIndex = fromJust (lookup chName chs)
        in  readIORef currFilter >>= \(haveInit, currFilterIdx) ->
            when ((not haveInit) || (currFilterIdx /= filterIndex)) (
                flushSerialPort port >> serialWrite port (T.encodeUtf8 . T.pack $ "1MU " ++ show (filterIndex + 1) ++ "\r") >>
                serialReadUntilChar port '\r' >>= \result ->
                case result of
                    "1MU +\r" -> writeIORef currFilter (True, filterIndex) >> return ()
                    v         -> throwIO (userError ("unknown response from ix71 dichroic turret: " ++ show v)))
    switchToFilter' (DummyFilterWheel name chs) chName =
        putStrLn ("Switched filter wheel " ++ T.unpack name ++ " to filter " ++ T.unpack chName)

fw103HMoveAbsoluteMessage :: Int -> ByteString
fw103HMoveAbsoluteMessage pos = runPut $
    mapM_ putWord8 [0x53, 0x04, 0x06, 0x00, 0xD0, 0x01, 0x01, 0x00] >>
    putWord32le (fromIntegral pos)

fw103HWaitUntilMotionStops :: SerialPort -> Int -> IO ()
fw103HWaitUntilMotionStops port targetPos =
    serialWrite port fw103HGetStatusMessage >>
    serialReadNBytes port 20 >>= \response ->
    let Right status = runGet getWord32le ((B.take 4 . B.drop 16) response)
        isInMotion = (0x00F0 .&. status) /= 0
        isHoming = (0x0200 .&. status) /= 0
        Right position = runGet getWord32le ((B.take 4 . B.drop 8) response)
        notAtCorrectPos = position /= (fromIntegral targetPos)
    in if (isInMotion || isHoming || notAtCorrectPos)
       then fw103HWaitUntilMotionStops port targetPos
       else return ()

fw103HWaitUntilHomingStops :: SerialPort -> IO ()
fw103HWaitUntilHomingStops port = fw103HWaitUntilMotionStops port 0

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
