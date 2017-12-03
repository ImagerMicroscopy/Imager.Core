{-# LANGUAGE BangPatterns #-}
module CameraImageProcessing (
    ImageVectorAndSize (..)
  , ExternalRearrangementFunc
  , rearrangeImageExternal
  , cRotateImageCW
  , cRotateImageCCW
  , cFlipImageHorizontal
  , cFlipImageVertical
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Word
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe

import AcquiredDataTypes
import SCCamera

data ImageVectorAndSize = ImageVectorAndSize {
                                 ivsVector :: !(Vector Word16)
                               , ivsSize   :: !(Int, Int)
                             }
data ImageSizeAndCoordinate = ImageSizeAndCoordinate {
                                  iscSize       :: !(Int, Int)
                                , iscCoordinate :: !(Int, Int)
                              }

rearrangeImageExternal :: [ExternalRearrangementFunc] -> ImageVectorAndSize -> ImageVectorAndSize
rearrangeImageExternal [] ivs = ivs
rearrangeImageExternal fs ivs = let f = foldl (.) id (map rearrangeImageExternalW fs)
                                 in  f ivs

rearrangeImageExternalW :: ExternalRearrangementFunc -> ImageVectorAndSize -> ImageVectorAndSize
rearrangeImageExternalW f (ImageVectorAndSize v (nRows, nCols))= unsafePerformIO $
    MV.unsafeNew (nRows * nCols) >>= \newIm ->
    alloca (\newNRowsPtr ->
    alloca (\newNColsPtr ->
    V.unsafeWith v (\imPtr ->
    MV.unsafeWith newIm (\newImPtr ->
        f imPtr (fromIntegral nRows) (fromIntegral nCols) newImPtr newNRowsPtr newNColsPtr >>
        (,) <$> peek newNRowsPtr <*> peek newNColsPtr)))) >>= \(newNRows, newNCols) ->
    V.unsafeFreeze newIm >>= \newVec ->
    return (ImageVectorAndSize newVec ((fromIntegral newNRows), (fromIntegral newNCols)))
