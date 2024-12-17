{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables, NumDecimals #-}

module RCSerialPort (
    SerialPort
  , SP.SerialPortSettings (..)
  , SP.defaultSerialSettings
  , SP.CommSpeed (..)
  , SP.StopBits (..)
  , RCSerialPortSettings (..)
  , TimeoutValue (..)
  , SerialPortDebugMode (..)
  , openSerialPort
  , closeSerialPort
  , serialWrite
  , serialWriteAndReadUntilTerminator
  , serialWriteAndReadUntilChar
  , serialWriteByteAndReadUntilChar
  , serialWriteByteAndReadByte
  , serialWriteAndReadUntilSequence
  , serialWriteAndReadAtLeastNBytes
  , serialReadAndWriteUntilCustom
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
import qualified System.Timeout as ST

import Utils.MiscUtils

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
    port <- SP.openSerial name settings
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

serialWriteByteAndReadByte :: SerialPort -> Word8 -> IO Word8
serialWriteByteAndReadByte port byte =
    withMVar (spMVar port) (\_ ->
        serialWrite' port (B.pack [byte]) >>
        serialRead port (\bs -> B.length bs >= 1) >>=
        pure . head . B.unpack)

serialWriteAndReadUntilSequence :: SerialPort -> ByteString -> ByteString -> IO ByteString
serialWriteAndReadUntilSequence port bs sequ =
    withMVar (spMVar port) (\_ ->
      serialWrite' port bs >>
      serialRead port (sequ `B.isSuffixOf`))

serialWriteAndReadAtLeastNBytes :: SerialPort -> ByteString -> Int -> IO ByteString
serialWriteAndReadAtLeastNBytes port bs n =
    withMVar (spMVar port) (\_ ->
        serialWrite' port bs >>
        let satisfiedP bs = B.length bs >= n
        in  serialRead port satisfiedP)

serialReadAndWriteUntilCustom :: SerialPort -> (ByteString -> Bool) -> IO ByteString
serialReadAndWriteUntilCustom = serialRead

serialRead :: SerialPort -> (ByteString -> Bool) -> IO ByteString
serialRead port satisfiedP =
    ST.timeout timeoutMicros (readWorker port satisfiedP B.empty) >>= \result ->
    case result of
        Just bytes -> possiblyPrintMessage port.spPortName port.spDebugMode "RECEIVED" bytes >>
                      pure (bytes)
        Nothing    -> throwIO (userError ("time limit exceeded reading from serial port " ++ show port.spPortName))
    where
        timeoutMicros = (toMillis port.spTimeout) * 1000
        readWorker :: SerialPort -> (ByteString -> Bool) -> ByteString -> IO ByteString
        readWorker p satisfiedP accum =
            B.append accum <$> SP.recv p.spPort 4096 >>= \newAccum ->
            if (not (satisfiedP newAccum))
              then readWorker p satisfiedP newAccum
              else pure newAccum

possiblyPrintMessage :: Text -> SerialPortDebugMode -> Text -> ByteString -> IO ()
possiblyPrintMessage portName dbgMode prefix msg =
    case dbgMode of
        SerialPortNoDebug     -> return ()
        SerialPortDebugBinary -> T.print "{}: {} {}\n" (portName, prefix, byteStringAsHex msg)
        SerialPortDebugText   -> T.print "{}: {} {}\n" (portName, prefix, T.decodeUtf8 msg)

flushSerialPort :: SerialPort -> IO ()
flushSerialPort (SerialPort port _ _ _ mVar) = withMVar mVar (\_ -> SP.flush port)
