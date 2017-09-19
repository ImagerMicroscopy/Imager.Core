module BinaryEncoding (
    shouldBinaryEncode
  , binaryEncode
) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize

import CuvettorTypes
import Detector
import MiscUtils

{-
Binary encoding is only supported for the messages containing data. The format of a
single message is fairly simple:
0: uint8 : 0    -- marker that clearly shows that this is a binary message
1: uint8 : 0
2: uint8 : 0    -- why 3 zero bytes? This avoids a bus error due to incorrect alignment of putFloat64le on ARMv7 (GHC 7.10.3)
3: uint16 : 11014
5: uint32 : total size of message including full header
9: uint32 : nRows
13: uint32 : nCols
17: uint8 : numType
18: uint32 : nDataSets
22: uint16: nDatasetsOfEachType
24: fp64[nDataSets] : timeStamps
    uint8[nRows * nCols * nDataSets] : actual data

binaryEncode generates a single message but returns the components in a list
that would produce the message when concatenated.
-}

shouldBinaryEncode :: ResponseMessage -> Bool
shouldBinaryEncode (AcquiredDataResponse _) = True
shouldBinaryEncode (AsyncAcquiredData _) = True
shouldBinaryEncode (Wavelengths _) = True
shouldBinaryEncode _ = False

binaryEncode :: ResponseMessage -> [ByteString]
binaryEncode (AcquiredDataResponse d) = encodeAcquiredData [[d]]
binaryEncode (AsyncAcquiredData ds) = encodeAcquiredData ds
binaryEncode (Wavelengths d) = encodeAcquiredData [[d]]
binaryEncode _ = error "no binary encoding for this type"

encodeAcquiredData :: [[AcquiredData]] -> [ByteString]
encodeAcquiredData [] = [encodeHeader 22 0 0 (encodedNumType UINT16) []]
encodeAcquiredData acqs = let header = encodeHeader messageLength nRows nCols numType timeStamps
                          in header : acqBytes
  where
      messageLength = 19 + (length concatenatedAcqs) * 8 + sum (map B.length acqBytes)
      concatenatedAcqs = concat acqs
      timeStamps = map (timeSpecAsDouble . acqTimeStamp) concatenatedAcqs
      nRows = acqNRows (head concatenatedAcqs)
      nCols = acqNCols (head concatenatedAcqs)
      numType = encodedNumType $ acqNumType (head concatenatedAcqs)
      acqBytes = map acqData concatenatedAcqs

encodeHeader :: Int -> Int -> Int -> Int -> [Double] -> ByteString
encodeHeader messageLength nRows nCols numType timeStamps =
    runPut $
        putWord16le 11014 >>
        putWord32le (fromIntegral messageLength) >>
        putWord32le (fromIntegral nRows) >>
        putWord32le (fromIntegral nCols) >>
        putWord8 (fromIntegral numType) >>
        putWord32le (fromIntegral $ length timeStamps) >>
        mapM_ putFloat64le timeStamps

encodedNumType :: NumberType -> Int
encodedNumType UINT8 = 2
encodedNumType UINT16 = 0
encodedNumType FP64 = 1
