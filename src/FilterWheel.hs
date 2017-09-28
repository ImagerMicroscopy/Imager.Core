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
import qualified System.Timeout as ST

import EquipmentMessaging
import EquipmentTypes
import MiscUtils
import RCSerialPort

filterWheelName :: Equipment -> Text
filterWheelName (ThorlabsFW103H name _ _) = name
filterWheelName (ThorlabsFW102C name _ _) = name
filterWheelName (SutterLambda10B name _ _ ) = name
filterWheelName (OlympusIX71Dichroic name _ _ _) = name
filterWheelName (DummyFilterWheel name _) = name

filterWheelChannels :: Equipment -> [Text]
filterWheelChannels (ThorlabsFW103H _ chs _) = map fst chs
filterWheelChannels (ThorlabsFW102C _ chs _) = map fst chs
filterWheelChannels (SutterLambda10B _ chs _) = map fst chs
filterWheelChannels (OlympusIX71Dichroic _ chs _ _) = map fst chs
filterWheelChannels (DummyFilterWheel _ chs) = map fst chs

filterWheelHasChannel :: Equipment -> Text -> Bool
filterWheelHasChannel w c = c `elem` (filterWheelChannels w)

switchFilterWheel :: [Equipment] -> Text -> Text -> IO ()
switchFilterWheel fws fwName fName =
    let filterWheel = head (filter (\fw -> filterWheelName fw == fwName) fws)
    in ST.timeout (floor 10e6) (switchToFilter filterWheel fName) >>= \result ->
       case result of
           Nothing -> throwIO (userError ("timeout communicating with " ++ T.unpack (filterWheelName filterWheel)))
           Just v -> return v

switchToFilter :: Equipment -> Text -> IO ()
switchToFilter fw chName | not (filterWheelHasChannel fw chName) = throwIO (userError "no matching channel for filter wheel")
                         | otherwise = switchToFilter' fw chName
  where
    switchToFilter' :: Equipment -> Text -> IO ()
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
