{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Utils.MiscUtils where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as SB
import qualified Data.ByteString.Lazy as LB
import Data.Either
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as NS
import Numeric
import System.IO
import System.IO.Unsafe
import System.Clock
import System.Hardware.Serialport hiding (timeout)
import System.Timeout

sequenceExcept :: [IO (Either String ())] -> IO (Either String ())
sequenceExcept [] = return (Right ())
sequenceExcept (a : as) = a >>= \result ->
                            case result of
                              Left e  -> return (Left e)
                              Right _ -> sequenceExcept as

sequenceEither :: [Either a ()] -> Either a ()
sequenceEither [] = return ()
sequenceEither xs = let (ls, rs) = partitionEithers xs
               in if (null ls) then Right (head rs) else Left (head ls)

mapFirst :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFirst f = map (\(a, b) -> (f a, b))

displayStringThenError :: String -> IO a
displayStringThenError s = putStrLn "ERROR:" >> putStrLn s >>
                           putStrLn "Press return to continue" >>
                           getLine >> error "terminating"

byteStringFromVector :: forall a . Storable a => Vector a -> ByteString
byteStringFromVector v = unsafePerformIO $
    let sizeOfElem = sizeOf (undefined :: a)
        nElems = V.length v
        nBytes = nElems * sizeOfElem in
    V.unsafeWith v $ \vecPtr ->
    mallocBytes nBytes >>= \bsPtr ->
    copyBytes (castPtr bsPtr) vecPtr nBytes >>
    SB.unsafePackMallocCStringLen (bsPtr, nBytes)

timeSpecAsDouble :: TimeSpec -> Double
timeSpecAsDouble ts = (*) 1.0e-9 . fromIntegral . toNanoSecs $ ts

readFromSerialUntilChar :: SerialPort -> Char -> IO ByteString
readFromSerialUntilChar port c = readUntil' port (fromIntegral . fromEnum $ c) B.empty
    where
        readUntil' :: SerialPort -> Word8 -> ByteString -> IO ByteString
        readUntil' port c accum | (not $ B.null accum) && (B.last accum == c) = return accum
                                | otherwise         = recv port 100 >>= \msg ->
                                                      readUntil' port c (accum <> msg)

readAtLeastNBytesFromSerial :: SerialPort -> Int -> IO ByteString
readAtLeastNBytesFromSerial port n = readBytes port n B.empty
  where
    readBytes p n accum = recv port 100 >>= \newBytes ->
                    let accum' = accum <> newBytes
                    in if (B.length accum' >= n)
                       then return accum'
                       else readBytes port n accum'

openSerialWithErrorMsg :: FilePath -> SerialPortSettings -> IO SerialPort
openSerialWithErrorMsg p s =
    catch (openSerial p s)
        (\(e :: IOException) -> putStrLn "error opening serial port:" >>
                                putStrLn (displayException e) >>
                                putStrLn "press return to close" >> getLine >>
                                error (show e))

debugSend :: SerialPort -> ByteString -> IO ()
debugSend p bs = putStrLn ("sending " ++ byteStringAsHex bs) >> send p bs >> return ()

data QueryServerParams a = QueryServerParams {
                               qspAddress :: !String
                             , qspPort :: !Int
                             , qspMaxMessageSize :: !Int
                             , qspIsMessageCompleteFunc :: LB.ByteString -> Bool
                             , qspMessageParser :: LB.ByteString -> a
                         }

queryServer :: QueryServerParams a -> ByteString -> IO a
queryServer (QueryServerParams addr port maxSize completeP messageParser) msg = withSocketsDo $
    getAddrInfo (Just hints) (Just addr) (Just (show port)) >>= \(serverAddr : _) ->
    bracket (socket (addrFamily serverAddr) Stream defaultProtocol) (close) (\sock -> do
        connect sock (addrAddress serverAddr)
        NS.sendAll sock msg
        messageParser <$> recvMessage sock LB.empty)
    where
        hints = defaultHints {addrSocketType = Stream , addrFamily = AF_INET}
        recvMessage :: Socket -> LB.ByteString -> IO LB.ByteString
        recvMessage sock accum =
            NS.recv sock 4096 >>= \buf ->
            when (B.length buf == 0) (
                throwIO (userError ("connection to " ++ addr ++ " closed unexpectedly"))) >>
            let accum' = accum <> (LB.fromStrict buf)
            in  when (LB.length accum' > fromIntegral maxSize) (
                    throwIO (userError "message length exceeds max size")) >>
                if (completeP accum')
                then return accum'
                else recvMessage sock accum'

isCompleteJSONObject :: LB.ByteString -> Bool
isCompleteJSONObject msg = case (decode msg :: Maybe Value) of
                               Just (Object _) -> True
                               _               -> False

decodeJSONObject :: (FromJSON a) => LB.ByteString -> a
decodeJSONObject msg = case (decode msg) of
                           Just v -> v
                           _      -> throw (userError ("couldn't decode message " ++ (T.unpack . T.decodeUtf8 . LB.toStrict $ msg)))

resetSystemSleepTimer :: IO ()
#ifdef WINDOWS
resetSystemSleepTimer = cSetThreadExecutionState gES_SYSTEM_REQUIRED >> return ()
foreign import stdcall unsafe "Windows.h SetThreadExecutionState"
    cSetThreadExecutionState :: Word32 -> IO Word32
gES_SYSTEM_REQUIRED :: Word32
gES_SYSTEM_REQUIRED = 0x00000001
#else
resetSystemSleepTimer = return ()
#endif

byteStringAsHex :: ByteString -> String
byteStringAsHex = concat . intersperse " " . map showByte . B.unpack
    where
        showByte :: Word8 -> String
        showByte b = (showHex (b `shiftR` 4) . showHex (b .&. 0x0F)) ""

byteStringAsString :: ByteString -> String
byteStringAsString = T.unpack . T.decodeUtf8

nodups :: Eq a => [a] -> Bool
nodups xs = (nub xs) == xs

fromLeft :: Either a b -> a
fromLeft (Left a) = a

fromRight :: Either a b -> b
fromRight (Right b) = b

within :: (Ord a) => a -> a -> a -> Bool
within a b c = (a >= b) && (a <= c)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a
snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b
thd3 :: (a, b, c) -> c
thd3 (a, b, c) = c

concatMaybes :: (Eq a) => [Maybe a] -> Maybe [a]
concatMaybes ms | Nothing `elem` ms = Nothing
                | otherwise = Just (map fromJust ms)

fromCDouble (CDouble d) = d
