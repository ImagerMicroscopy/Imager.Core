{-# LANGUAGE BangPatterns #-}
module CameraImageProcessing where

import Control.Monad
import Control.Monad.ST
import Data.Word
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

type ImageProcessingFunc = (Vector Word16, (Int, Int)) -> (Vector Word16 , (Int, Int))
type CoordinateRearrangementFunc = (Int, Int) -> (Int, Int) -> (Int, Int)

rotateCW :: ImageProcessingFunc
rotateCW (v, (nRows, nCols)) = (rearrangeImage rotateCWIndex' v (nRows, nCols) (nCols, nRows), (nCols, nRows))
rotateCCW :: ImageProcessingFunc
rotateCCW (v, (nRows, nCols)) = (rearrangeImage rotateCCWIndex' v (nRows, nCols) (nCols, nRows), (nCols, nRows))
flipHorizontal :: ImageProcessingFunc
flipHorizontal (v, (nRows, nCols)) = (rearrangeImage flipHorizontalIndex' v (nRows, nCols) (nRows, nCols), (nRows, nCols))
flipVertical :: ImageProcessingFunc
flipVertical (v, (nRows, nCols)) = (rearrangeImage flipVerticalIndex' v (nRows, nCols) (nRows, nCols), (nRows, nCols))

rearrangeImage :: CoordinateRearrangementFunc -> Vector Word16 -> (Int, Int) -> (Int, Int) -> Vector Word16
rearrangeImage f v (nRows, nCols) (nRowsNew, nColsNew) =
    V.generate (nRows * nCols) (\idx ->
        let oldIndex = (rowColToLinear nRows . f (nRowsNew, nColsNew) . linearToRowCol nRowsNew) idx
        in  (v V.! oldIndex))

rotateCWIndex :: CoordinateRearrangementFunc    -- calculates where the pixel at (row, col) goes in the rotated image. (nRows, nCols) from original image!
rotateCWIndex (nRows, nCols) (row, col) = (nCols - 1 - col, row)
rotateCCWIndex  :: CoordinateRearrangementFunc  -- calculates where the pixel at (row, col) goes in the rotated image. (nRows, nCols) from original image!
rotateCCWIndex (nRows, nCols) (row, col) = (col, nRows - 1 - row)
flipHorizontalIndex :: CoordinateRearrangementFunc  -- calculates where the pixel at (row, col) goes in the flipped image. (nRows, nCols) from original image!
flipHorizontalIndex (nRows, nCols) (row, col) = (row, nCols - 1 - col)
flipVerticalIndex :: CoordinateRearrangementFunc  -- calculates where the pixel at (row, col) goes in the flipped image. (nRows, nCols) from original image!
flipVerticalIndex (nRows, nCols) (row, col) = (nRows - 1 - row, col)

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
