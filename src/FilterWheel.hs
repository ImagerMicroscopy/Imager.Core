{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module FilterWheel where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
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
        openSerial portName (defaultSerialSettings {commSpeed = CS115200}) >>= \port ->
        send port fw103HInitializationMessage >> send port fw103HEnableChannelMessage >>
        send port fw103HMoveHomeMessage >> return (ThorlabsFW103H name (validateChannels chs) port)
    openFilterWheel (ThorlabsFW102CDesc name portName chs) =
        ThorlabsFW102C name (validateChannels chs) <$> openSerial portName (defaultSerialSettings {commSpeed = CS115200})
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
           Nothing -> error ("timeout communicating with " ++ T.unpack (filterWheelName filterWheel))
           Just v -> return v

switchToFilter :: FilterWheel -> Text -> IO (Either String ())
switchToFilter fw chName | not (filterWheelHasChannel fw chName) = error "no matching channel for filter wheel"
                         | otherwise = switchToFilter' fw chName
  where
    switchToFilter' :: FilterWheel -> Text -> IO (Either String ())
    switchToFilter' (ThorlabsFW103H _ chs port) chName =
        let filterIndex = fromJust (lookup chName chs)
            wheelPos = (409600 `div` 6) * filterIndex -- Thorlabs:  1 turn represents 360 degrees which is 409600 micro steps
        in  send port (fw103HMoveAbsoluteMessage wheelPos) >> threadDelay (floor 150e3) >> return (Right ())
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

fw103HInitializationMessage :: ByteString
fw103HInitializationMessage = B.pack [0x18, 0x00, 0x00, 0x00, 0x50, 0x01]

fw103HEnableChannelMessage :: ByteString
fw103HEnableChannelMessage = B.pack [0x10, 0x02, 0x01, 0x01, 0x50, 0x01]

fw103HMoveHomeMessage :: ByteString
fw103HMoveHomeMessage = B.pack [0x43, 0x04, 0x01, 0x00, 0x50, 0x01]
