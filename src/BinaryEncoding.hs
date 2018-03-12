module BinaryEncoding (
    shouldBinaryEncode
  , binaryEncode
) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize
import Data.Word

import CuvettorTypes
import Detector
import MiscUtils

{-
Binary encoding is only supported for the messages containing data. Format:
0: uint16 : magic value 11014
2: uint32 : total size of message including full header
6: uint8 : numType
7: uint32 : nDataSets
11: uint64[nDataSets]: index of each dataset
    (uint32, uint32)[nDataSets]: size of each dataset
    fp64[nDataSets] : timeStamps
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
binaryEncode (AcquiredDataResponse d) = encodeAcquiredData [d]
binaryEncode (AsyncAcquiredData ds) = encodeAcquiredData ds
binaryEncode (Wavelengths d) = encodeAcquiredData [(0, d)]
binaryEncode _ = error "no binary encoding for this type"

encodeAcquiredData :: [(Word64, AcquiredData)] -> [ByteString]
encodeAcquiredData [] = [encodeHeader 11 [] [] (encodedNumType UINT16) []]
encodeAcquiredData acqs = let header = encodeHeader messageLength indices dataSizes numType timeStamps
                          in  header : acqBytes
  where
      messageLength = 11 + (length acqs) * (8 + 8 + 8) + sum (map B.length acqBytes)
      timeStamps = map (timeSpecAsDouble . acqTimeStamp . snd) acqs
      indices = map fst acqs
      dataSizes = map ((\a -> (acqNRows a, acqNCols a)) . snd) acqs
      numType = encodedNumType $ acqNumType (snd (head acqs))
      acqBytes = map (acqData . snd) acqs

encodeHeader :: Int -> [Word64] -> [(Int, Int)] -> Int -> [Double] -> ByteString
encodeHeader messageLength indices dataDims numType timeStamps =
    runPut $
        putWord16le 11014 >>
        putWord32le (fromIntegral messageLength) >>
        putWord8 (fromIntegral numType) >>
        putWord32le (fromIntegral $ length timeStamps) >>
        mapM_ putWord64le indices >>
        forM_ dataDims (\(nRows, nCols) ->
            putWord32le (fromIntegral nRows) >> putWord32le (fromIntegral nCols)) >>
        mapM_ putFloat64le timeStamps

encodedNumType :: NumberType -> Int
encodedNumType UINT8 = 2
encodedNumType UINT16 = 0
encodedNumType FP64 = 1
