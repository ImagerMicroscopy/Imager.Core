{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables, NumDecimals #-}

module RCSerialPort (

    SerialPort
  , SP.SerialPortSettings (..)
  , SP.defaultSerialSettings
  , SP.CommSpeed (..)
  , RCSerialPortSettings (..)
  , TimeoutValue (..)
  , SerialPortDebugMode (..)
  , openSerialPort
  , closeSerialPort
  , serialWrite
  , serialReadUntilTerminator
  , serialReadUntilChar
  , serialReadNBytes
  , flushSerialPort
)
where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Format as T
import Data.Word
import System.Clock
import qualified System.Hardware.Serialport as SP

import MiscUtils

data SerialPortDebugMode = SerialPortNoDebug
                         | SerialPortDebugBinary
                         | SerialPortDebugText

data RCSerialPortSettings = RCSerialPortSettings {
                                spsPortSettings :: !SP.SerialPortSettings
                              , spsTimeout :: !TimeoutValue
                              , spsDebugMode :: !SerialPortDebugMode
                          }

data SerialPort = SerialPort {
                      spPort :: !SP.SerialPort
                    , spPortName :: !Text
                    , spTimeout :: !TimeoutValue
                    , spDebugMode :: !SerialPortDebugMode
                  }

newtype TimeoutValue = TimeoutMillis {toMillis :: Int}

openSerialPort :: FilePath -> RCSerialPortSettings -> IO SerialPort
openSerialPort name (RCSerialPortSettings settings timeout debugMode) = do
    port <- catch (SP.openSerial name settings)
                (\(e :: IOException) -> error ("Unable to open serial port " ++ name ++ "\nPress return to continue") >>
                                        getLine >> error "unable to open")
    return (SerialPort port (T.pack name) timeout debugMode)

closeSerialPort :: SerialPort -> IO ()
closeSerialPort (SerialPort port _ _ _) = SP.closeSerial port

serialWrite :: SerialPort -> ByteString -> IO ()
serialWrite port bs
    | B.null bs = return ()
    | otherwise = SP.send (spPort port) bs >>= \nBytesSent ->
                  serialWrite port (B.drop nBytesSent bs)

serialReadUntilTerminator :: SerialPort -> Word8 -> IO ByteString
serialReadUntilTerminator p terminator = serialRead p satisfiedP
    where satisfiedP bs = B.last bs == terminator

serialReadUntilChar :: SerialPort -> Char -> IO ByteString
serialReadUntilChar p c = serialReadUntilTerminator p (fromIntegral . fromEnum $ c)

serialReadNBytes :: SerialPort -> Int -> IO ByteString
serialReadNBytes p n = serialRead p satisfiedP
    where satisfiedP bs = B.length bs == n

serialRead :: SerialPort -> (ByteString -> Bool) -> IO ByteString
serialRead port satisfiedP =
    fromNanoSecs . (+) (fromIntegral timeoutMillis * 1e6) . toNanoSecs <$> getTime Monotonic >>= \deadline ->
    readWorker port satisfiedP B.empty deadline
    where
        timeoutMillis = toMillis $ spTimeout port
        readWorker :: SerialPort -> (ByteString -> Bool) -> ByteString -> TimeSpec -> IO ByteString
        readWorker p@(SerialPort port portName _ debugMode) satisfiedP accum deadline =
            getTime Monotonic >>= \now ->
            when (now > deadline)
                (throwIO (userError ("time limit exceeded reading from serial port " ++ show portName))) >>
            B.append accum <$> SP.recv port 4096 >>= \newAccum ->
            if (not $ satisfiedP newAccum)
              then readWorker p satisfiedP newAccum deadline
              else possiblyPrintMessage portName debugMode newAccum >> return newAccum
        possiblyPrintMessage :: Text -> SerialPortDebugMode -> ByteString -> IO ()
        possiblyPrintMessage portName dbgMode msg =
            case dbgMode of
                SerialPortNoDebug     -> return ()
                SerialPortDebugBinary -> T.print "{}:RECEIVED {}" (portName, byteStringAsHex msg)
                SerialPortDebugText   -> T.print "{}:RECEIVED {}" (portName, T.decodeUtf8 msg)

flushSerialPort :: SerialPort -> IO ()
flushSerialPort (SerialPort port _ _ _) = SP.flush port
