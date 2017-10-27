{-# LANGUAGE BangPatterns #-}
module CameraImageProcessing (
    ImageVectorAndSize (..)
  , CoordinateRearrangementFunc
  , rearrangeImage
  , rotateCWIndex'
  , rotateCCWIndex'
  , flipHorizontalIndex'
  , flipVerticalIndex'
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Word
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

data ImageVectorAndSize = ImageVectorAndSize {
                                 ivsVector :: !(Vector Word16)
                               , ivsSize   :: !(Int, Int)
                             }
data ImageSizeAndCoordinate = ImageSizeAndCoordinate {
                                  iscSize       :: !(Int, Int)
                                , iscCoordinate :: !(Int, Int)
                              }

type CoordinateRearrangementFunc = ImageSizeAndCoordinate -> ImageSizeAndCoordinate

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
