module Encodings.BinaryEncoding (
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
import Detectors.Detector
import Equipment.EquipmentTypes
import Utils.MiscUtils

{-
Binary encoding is only supported for the messages containing data. Format:
0: uint16 : magic value 11014
2: uint32 : total size of message including full header
6: uint8 : numType
7: uint32 : nDataSets
11: uint64[nDataSets]: index of each dataset
    fp64[3 * nDataSets]: stage position at each dataset
    utf-8 string[nDataSets]: acquisition type name of each dataset
    utf-8 string [nDataSets]: name of detector that acquired dataset
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
binaryEncode (AcquiredDataResponse d) = encodeAcquiredData d
binaryEncode (AsyncAcquiredData ds) = encodeAcquiredData ds
binaryEncode (Wavelengths d) = encodeAcquiredData [(AcquisitionMetaData 0 (StagePosition (-1.0) (-1.0) (-1.0) False 0) "DUMMY", d)]
binaryEncode _ = error "no binary encoding for this type"

encodeAcquiredData :: [(AcquisitionMetaData, AcquiredData)] -> [ByteString]
encodeAcquiredData [] = error "Encoding empty data"
encodeAcquiredData acqs = let header = encodeHeader messageLength indices stagePositions acqTypeNames detectorNames dataSizes numType timeStamps
                          in  header : acqBytes
  where
      metadatas = map fst acqs
      datas = map snd acqs
      messageLength = 11 + (length acqs) * (8 + 24 + 8 + 8) + lengthOfEncodedAcquisitionNames + lengthOfEncodedDetectorNames + sum (map B.length acqBytes)
      lengthOfEncodedAcquisitionNames = sum (map ((+) 1  . B.length) acqTypeNames)
      lengthOfEncodedDetectorNames = sum (map ((+) 1 . B.length) detectorNames)
      timeStamps = map (timeSpecAsDouble . acqTimeStamp) datas
      indices = map amdSequence metadatas
      stagePositions = map amdStagePosition metadatas
      acqTypeNames = map (B.take 255 . T.encodeUtf8 . amdAcquisitionTypename) metadatas
      detectorNames = map (B.take 255 . T.encodeUtf8 . acqDetectorName) datas
      dataSizes = map ((\a -> (acqNRows a, acqNCols a))) datas
      numType = encodedNumType $ acqNumType (head datas)
      acqBytes = map acqData datas

encodeHeader :: Int -> [Word64] -> [StagePosition] -> [ByteString] -> [ByteString] -> [(Int, Int)] -> Int -> [Double] -> ByteString
encodeHeader messageLength indices stagePoss acqTypeNames detectorNames dataDims numType timeStamps =
    runPut $
        putWord16le 11014 >>
        putWord32le (fromIntegral messageLength) >>
        putWord8 (fromIntegral numType) >>
        putWord32le (fromIntegral $ length timeStamps) >>
        mapM_ putWord64le indices >>
        forM_ stagePoss (\(StagePosition x y z _ _) -> putFloat64le x >> putFloat64le y >> putFloat64le z) >>
        forM_ acqTypeNames (\n -> putWord8 (fromIntegral $ B.length n) >> putByteString n) >>
        forM_ detectorNames (\n -> putWord8 (fromIntegral $ B.length n) >> putByteString n) >>
        forM_ dataDims (\(nRows, nCols) ->
            putWord32le (fromIntegral nRows) >> putWord32le (fromIntegral nCols)) >>
        mapM_ putFloat64le timeStamps

encodedNumType :: NumberType -> Int
encodedNumType UINT8 = 2
encodedNumType UINT16 = 0
encodedNumType FP64 = 1
