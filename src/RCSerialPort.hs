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
  , serialWriteAndReadUntilTerminator
  , serialWriteAndReadUntilChar
  , serialWriteByteAndReadUntilChar
  , serialWriteAndReadAtLeastNBytes
  , flushSerialPort
)
where

import Control.Concurrent.MVar
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
                    , spMVar :: !(MVar ())
                  }

newtype TimeoutValue = TimeoutMillis {toMillis :: Int}

openSerialPort :: FilePath -> RCSerialPortSettings -> IO SerialPort
openSerialPort name (RCSerialPortSettings settings timeout debugMode) = do
    port <- catch (SP.openSerial name settings)
                (\(e :: IOException) -> displayStringThenError ("Unable to open serial port " ++ name))
    SerialPort port (T.pack name) timeout debugMode <$> newMVar ()

closeSerialPort :: SerialPort -> IO ()
closeSerialPort (SerialPort port _ _ _ _) = SP.closeSerial port

serialWrite :: SerialPort -> ByteString -> IO ()
serialWrite port bs
    | B.null bs = return ()
    | otherwise = withMVar (spMVar port) (\_ ->
                      serialWrite' port bs)

serialWrite' :: SerialPort -> ByteString -> IO ()
serialWrite' port bs
    | B.null bs = pure ()
    | otherwise =
          SP.send (spPort port) bs >>= \nBytesSent ->
          possiblyPrintMessage (spPortName port) (spDebugMode port) "SENT" (B.take nBytesSent bs) >>
          serialWrite' port (B.drop nBytesSent bs)

serialWriteAndReadUntilTerminator :: SerialPort -> ByteString -> Word8 -> IO ByteString
serialWriteAndReadUntilTerminator port bs term =
    withMVar (spMVar port) (\_ ->
        serialWrite' port bs >>
        serialReadUntilTerminator' port term)
    where
        serialReadUntilTerminator' :: SerialPort -> Word8 -> IO ByteString
        serialReadUntilTerminator' p terminator = serialRead p satisfiedP
            where satisfiedP bs = not (B.null bs) && (B.last bs == terminator)

serialWriteAndReadUntilChar :: SerialPort -> ByteString -> Char -> IO ByteString
serialWriteAndReadUntilChar port bs c = serialWriteAndReadUntilTerminator port bs (fromIntegral . fromEnum $ c)

serialWriteByteAndReadUntilChar :: SerialPort -> Word8 -> Char -> IO ByteString
serialWriteByteAndReadUntilChar port byte c = serialWriteAndReadUntilChar port (B.pack [byte]) c

serialWriteAndReadAtLeastNBytes :: SerialPort -> ByteString -> Int -> IO ByteString
serialWriteAndReadAtLeastNBytes port bs n =
    withMVar (spMVar port) (\_ ->
        serialWrite' port bs >>
        let satisfiedP bs = B.length bs >= n
        in  serialRead port satisfiedP)

serialRead :: SerialPort -> (ByteString -> Bool) -> IO ByteString
serialRead port satisfiedP =
    fromNanoSecs . (+) (fromIntegral timeoutMillis * 1e6) . toNanoSecs <$> getTime Monotonic >>= \deadline ->
    readWorker port satisfiedP B.empty deadline
    where
        timeoutMillis = toMillis $ spTimeout port
        readWorker :: SerialPort -> (ByteString -> Bool) -> ByteString -> TimeSpec -> IO ByteString
        readWorker p@(SerialPort port portName _ debugMode _) satisfiedP accum deadline =
            getTime Monotonic >>= \now ->
            when (now > deadline)
                (throwIO (userError ("time limit exceeded reading from serial port " ++ show portName))) >>
            B.append accum <$> SP.recv port 4096 >>= \newAccum ->
            if (not $ satisfiedP newAccum)
              then readWorker p satisfiedP newAccum deadline
              else possiblyPrintMessage portName debugMode "RECEIVED" newAccum >> return newAccum

possiblyPrintMessage :: Text -> SerialPortDebugMode -> Text -> ByteString -> IO ()
possiblyPrintMessage portName dbgMode prefix msg =
    case dbgMode of
        SerialPortNoDebug     -> return ()
        SerialPortDebugBinary -> T.print "{}: {} {}\n" (portName, prefix, byteStringAsHex msg)
        SerialPortDebugText   -> T.print "{}: {} {}\n" (portName, prefix, T.decodeUtf8 msg)

flushSerialPort :: SerialPort -> IO ()
flushSerialPort (SerialPort port _ _ _ mVar) = withMVar mVar (\_ -> SP.flush port)
