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

data ImageVectorAndSize = ImageVectorAndSize {
                                 ivsVector :: !(Vector Word16)
                               , ivsSize   :: !(Int, Int)
                             }
data ImageSizeAndCoordinate = ImageSizeAndCoordinate {
                                  iscSize       :: !(Int, Int)
                                , iscCoordinate :: !(Int, Int)
                              }

type CoordinateRearrangementFunc = ImageSizeAndCoordinate -> ImageSizeAndCoordinate
type ExternalRearrangementFunc = (Ptr Word16 -> CInt -> CInt -> Ptr Word16 -> Ptr CInt -> Ptr CInt -> IO ())

rearrangeImage :: CoordinateRearrangementFunc -> ImageVectorAndSize -> ImageVectorAndSize
rearrangeImage f (ImageVectorAndSize v (nRows, nCols)) = ImageVectorAndSize newVec (nRowsNew, nColsNew)
    where
        ImageSizeAndCoordinate (nRowsNew, nColsNew) _ = f (ImageSizeAndCoordinate (nRows, nCols) (0, 0))
        newVec = V.generate (nRowsNew * nColsNew) (\idx ->
                     let oldIndex = (rowColToLinear nRows . iscCoordinate . f . ImageSizeAndCoordinate (nRowsNew, nColsNew) . linearToRowCol nRowsNew) idx
                     in  (v V.! oldIndex))

rotateCWIndex :: CoordinateRearrangementFunc    -- calculates where the pixel at (row, col) goes in the rotated image. (nRows, nCols) from original image!
rotateCWIndex (ImageSizeAndCoordinate (nRows, nCols) (row, col)) = ImageSizeAndCoordinate (nCols, nRows) (nCols - 1 - col, row)
rotateCCWIndex  :: CoordinateRearrangementFunc  -- calculates where the pixel at (row, col) goes in the rotated image. (nRows, nCols) from original image!
rotateCCWIndex (ImageSizeAndCoordinate (nRows, nCols) (row, col)) = ImageSizeAndCoordinate (nCols, nRows) (col, nRows - 1 - row)
flipHorizontalIndex :: CoordinateRearrangementFunc  -- calculates where the pixel at (row, col) goes in the flipped image. (nRows, nCols) from original image!
flipHorizontalIndex (ImageSizeAndCoordinate (nRows, nCols) (row, col)) = ImageSizeAndCoordinate (nRows, nCols) (row, nCols - 1 - col)
flipVerticalIndex :: CoordinateRearrangementFunc  -- calculates where the pixel at (row, col) goes in the flipped image. (nRows, nCols) from original image!
flipVerticalIndex (ImageSizeAndCoordinate (nRows, nCols) (row, col)) = ImageSizeAndCoordinate (nRows, nCols) (nRows - 1 - row, col)

rotateCWIndex' :: CoordinateRearrangementFunc    -- calculates where the pixel at (row, col) in the rotated image was in the original image. (nRows, nCols) from rotated image!
rotateCWIndex' = rotateCCWIndex
rotateCCWIndex'  :: CoordinateRearrangementFunc  -- calculates where the pixel at (row, col) in the rotated image was in the original image. (nRows, nCols) from rotated image!
rotateCCWIndex' = rotateCWIndex
flipHorizontalIndex' :: CoordinateRearrangementFunc  -- calculates where the pixel at (row, col) in the flipped image was in the original image. (nRows, nCols) from flipped image!
flipHorizontalIndex' = flipHorizontalIndex
flipVerticalIndex' :: CoordinateRearrangementFunc  -- calculates where the pixel at (row, col) in the flipped image was in the original image. (nRows, nCols) from flipped image!
flipVerticalIndex' = flipVerticalIndex

rowColToLinear :: Int -> (Int, Int) -> Int
rowColToLinear !nRows (!row, !col) = col * nRows + row

linearToRowCol :: Int -> Int -> (Int, Int)
linearToRowCol !nRows !i = (i `rem` nRows, i `quot` nRows)

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

foreign import ccall unsafe "RotateImageCW"
    cRotateImageCW :: Ptr Word16 -> CInt -> CInt -> Ptr Word16 -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "RotateImageCCW"
    cRotateImageCCW :: Ptr Word16 -> CInt -> CInt -> Ptr Word16 -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "FlipImageHorizontal"
    cFlipImageHorizontal :: Ptr Word16 -> CInt -> CInt -> Ptr Word16 -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "FlipImageVertical"
    cFlipImageVertical :: Ptr Word16 -> CInt -> CInt -> Ptr Word16 -> Ptr CInt -> Ptr CInt -> IO ()
