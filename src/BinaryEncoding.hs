module BinaryEncoding (
    shouldBinaryEncode
  , binaryEncode
) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize
import qualified Data.Text.Encoding as T
import Data.Word
import Debug.Trace
import AcquiredDataTypes
import CuvettorTypes
import Detector
import EquipmentTypes
import MiscUtils

{-
Binary encoding is only supported for the messages containing data. Format:
0: uint16 : magic value 11014
2: uint32 : total size of message including full header
6: uint8 : numType
7: uint32 : nDataSets
11: uint64[nDataSets]: index of each dataset
    fp64[3 * nDataSets]: stage position at each dataset
    utf-8 string[nDataSets]: acquisition type name of each dataset
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
binaryEncode (Wavelengths d) = encodeAcquiredData [(AcquisitionMetaData 0 (StagePosition (-1.0) (-1.0) (-1.0) False 0) "DUMMY", d)]
binaryEncode _ = error "no binary encoding for this type"

encodeAcquiredData :: [(AcquisitionMetaData, AcquiredData)] -> [ByteString]
encodeAcquiredData [] = [encodeHeader 11 [] [] [] [] (encodedNumType UINT16) []]
encodeAcquiredData acqs = let header = encodeHeader messageLength indices stagePositions acqTypeNames dataSizes numType timeStamps
                          in  header : acqBytes
  where
      messageLength = 11 + (length acqs) * (8 + 24 + 8 + 8) + lengthOfEncodedAcquisitionNames + sum (map B.length acqBytes)
      lengthOfEncodedAcquisitionNames = sum (map ((+) 1  . B.length) acqTypeNames)
      timeStamps = map (timeSpecAsDouble . acqTimeStamp . snd) acqs
      indices = map (amdSequence . fst) acqs
      stagePositions = map (amdStagePosition . fst) acqs
      acqTypeNames = map (B.take 255 . T.encodeUtf8 . amdAcquisitionTypename . fst) acqs
      dataSizes = map ((\a -> (acqNRows a, acqNCols a)) . snd) acqs
      numType = encodedNumType $ acqNumType (snd (head acqs))
      acqBytes = map (acqData . snd) acqs

encodeHeader :: Int -> [Word64] -> [StagePosition] -> [ByteString] -> [(Int, Int)] -> Int -> [Double] -> ByteString
encodeHeader messageLength indices stagePoss acqTypeNames dataDims numType timeStamps =
    runPut $
        putWord16le 11014 >>
        putWord32le (fromIntegral messageLength) >>
        putWord8 (fromIntegral numType) >>
        putWord32le (fromIntegral $ length timeStamps) >>
        mapM_ putWord64le indices >>
        forM_ stagePoss (\(StagePosition x y z _ _) -> putFloat64le x >> putFloat64le y >> putFloat64le z) >>
        forM_ acqTypeNames (\n -> putWord8 (fromIntegral $ B.length n) >> putByteString n) >>
        forM_ dataDims (\(nRows, nCols) ->
            putWord32le (fromIntegral nRows) >> putWord32le (fromIntegral nCols)) >>
        mapM_ putFloat64le timeStamps

encodedNumType :: NumberType -> Int
encodedNumType UINT8 = 2
encodedNumType UINT16 = 0
encodedNumType FP64 = 1
