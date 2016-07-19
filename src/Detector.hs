{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Detector where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as SB
import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import System.IO.Unsafe

type ExposureTime = Double
type Gain = Double
type NMeasurementsToAverage = Int

data NumberType = UINT16
                | FP64
                deriving (Show)

data AcquiredData = AcquiredData {
                        acqNRows :: !Int
                      , acqNCols :: !Int
                      , acqData :: !ByteString
                      , acqNumType :: !NumberType
                  } deriving (Show)

class Detector a where
    acquireData :: a -> ExposureTime -> Gain -> NMeasurementsToAverage -> IO (Either String AcquiredData)

byteStringFromVector :: forall a . Storable a => Vector a -> ByteString
byteStringFromVector v = unsafePerformIO $
    let sizeOfElem = sizeOf (undefined :: a)
        nElems = V.length v
        nBytes = nElems * sizeOfElem in
    V.unsafeWith v $ \vecPtr ->
    mallocBytes nBytes >>= \bsPtr ->
    copyBytes (castPtr bsPtr) vecPtr nBytes >>
    SB.unsafePackMallocCStringLen (bsPtr, nBytes)
