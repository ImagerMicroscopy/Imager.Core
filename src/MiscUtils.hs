{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module MiscUtils where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as SB
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr
import Numeric
import System.IO.Unsafe
import System.Clock
import System.Hardware.Serialport

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

within :: (Ord a) => a -> a -> a -> Bool
within a b c = (a >= b) && (a <= c)
