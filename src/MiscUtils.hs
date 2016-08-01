{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module MiscUtils where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as SB
import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import System.IO.Unsafe
import System.Clock

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
timeSpecAsDouble ts = (*) 1.0e-9 . fromIntegral . nsec $ ts

fromLeft :: Either a b -> a
fromLeft (Left a) = a

within :: (Ord a) => a -> a -> a -> Bool
within a b c = (a >= b) && (a <= c)
