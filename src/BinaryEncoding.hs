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
0: uint8 : 0
1: uint16 : 11014
3: uint32 : total size of message including full header
7: uint32 : nRows
11: uint32 : nCols
15: uint8 : numType
16: uint32 : nDataSets
20: uint16: nDatasetsOfEachType
22: fp64[nDataSets] : timeStamps
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
encodeAcquiredData [] = [encodeHeader 22 0 0 0 (encodedNumType UINT16) []]
encodeAcquiredData acqs = encodeHeader messageLength nRows nCols nDatasetsOfEachType numType timeStamps : acqBytes
  where
      messageLength = 22 + (length acqs) * 8 + sum (map B.length acqBytes)
      concatenatedAcqs = concat acqs
      timeStamps = map (timeSpecAsDouble . acqTimeStamp) concatenatedAcqs
      nRows = acqNRows (head concatenatedAcqs)
      nCols = acqNCols (head concatenatedAcqs)
      nDatasetsOfEachType = length (head acqs)
      numType = encodedNumType $ acqNumType (head concatenatedAcqs)
      acqBytes = map acqData concatenatedAcqs

encodeHeader :: Int -> Int -> Int -> Int -> Int -> [Double] -> ByteString
encodeHeader messageLength nRows nCols nDatasetsOfEachType numType timeStamps =
    runPut $
        putWord8 0 >> putWord16le 11014 >>
        putWord32le (fromIntegral messageLength) >>
        putWord32le (fromIntegral nRows) >>
        putWord32le (fromIntegral nCols) >>
        putWord8 (fromIntegral numType) >>
        putWord32le (fromIntegral $ length timeStamps) >>
        putWord16le (fromIntegral nDatasetsOfEachType) >>
        mapM_ putFloat64le timeStamps

encodedNumType :: NumberType -> Int
encodedNumType UINT16 = 0
encodedNumType FP64 = 1
