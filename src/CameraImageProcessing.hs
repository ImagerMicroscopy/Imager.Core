{-# LANGUAGE BangPatterns #-}
module CameraImageProcessing (
    ExternalRearrangementFunc
  , rearrangeImageExternal
  , processingFunc
  ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Data.Word
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe

import AcquiredDataTypes
import SCCamera

data ImageProcessingOperation = IPORotateCW
                              | IPORotateCCW
                              | IPOFlipHorizontal
                              | IPOFlipVertical
                              deriving (Read, Show)

processingFunc :: ImageProcessingOperation -> ExternalRearrangementFunc
processingFunc IPORotateCW = cRotateImageCW
processingFunc IPORotateCCW = cRotateImageCCW
processingFunc IPOFlipHorizontal = cFlipImageHorizontal
processingFunc IPOFlipVertical = cFlipImageVertical

rearrangeImageExternal :: [ExternalRearrangementFunc] -> AcquiredData -> AcquiredData
rearrangeImageExternal [] ivs = ivs
rearrangeImageExternal fs ivs = let f = foldl (.) id (map rearrangeImageExternalW fs)
                                in  f ivs

rearrangeImageExternalW :: ExternalRearrangementFunc -> AcquiredData -> AcquiredData
rearrangeImageExternalW f (AcquiredData nRows nCols ts bytes numType)
    | numType /= UINT16 = error "can only process UINT16 images"
    | otherwise         = unsafePerformIO $
        mallocForeignPtrBytes (B.length bytes) >>= \newBytesFPtr ->
        withForeignPtr newBytesFPtr (\newPtr ->
        B.unsafeUseAsCString bytes (\oldPtr ->
        alloca (\newNRowsPtr ->
        alloca (\newNColsPtr ->
        f (castPtr oldPtr) (fromIntegral nRows) (fromIntegral nCols) (castPtr newPtr) newNRowsPtr newNColsPtr >>
        AcquiredData <$> (fromIntegral <$> peek newNRowsPtr) <*> (fromIntegral <$> peek newNColsPtr)
                     <*> pure ts <*> pure (B.fromForeignPtr newBytesFPtr 0 (B.length bytes)) <*> pure numType))))
