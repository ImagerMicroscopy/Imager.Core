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
rotateCW (v, (nRows, nCols)) = (rearrangeImage rotateCWIndex' v (nRows, nCols), (nCols, nRows))

rotateCCW :: ImageProcessingFunc
rotateCCW (v, (nRows, nCols)) = (rearrangeImage rotateCCWIndex' v (nRows, nCols), (nCols, nRows))

rearrangeImage :: CoordinateRearrangementFunc -> Vector Word16 -> (Int, Int) -> Vector Word16
rearrangeImage f v (nRows, nCols) =
    V.generate (nRows * nCols) (\idx ->
        let oldIndex = (rowColToLinear nRows . f (nCols, nRows) . linearToRowCol nCols) idx
        in  (v V.! oldIndex))

rotateCWIndex :: CoordinateRearrangementFunc    -- calculates where the pixel at (row, col) goes in the rotated image. (nRows, nCols) from original image!
rotateCWIndex (nRows, nCols) (row, col) = (nCols - 1 - col, row)
rotateCCWIndex  :: CoordinateRearrangementFunc  -- calculates where the pixel at (row, col) goes in the rotated image. (nRows, nCols) from original image!
rotateCCWIndex (nRows, nCols) (row, col) = (col, nRows - 1 - row)

rotateCWIndex' :: CoordinateRearrangementFunc    -- calculates where the pixel at (row, col) in the rotated image was in the original image. (nRows, nCols) from rotated image!
rotateCWIndex' = rotateCCWIndex
rotateCCWIndex'  :: CoordinateRearrangementFunc  -- calculates where the pixel at (row, col) in the rotated image was in the original image. (nRows, nCols) from rotated image!
rotateCCWIndex' = rotateCWIndex

rowColToLinear :: Int -> (Int, Int) -> Int
rowColToLinear !nRows (!row, !col) = col * nRows + row

linearToRowCol :: Int -> Int -> (Int, Int)
linearToRowCol !nRows !i = (i `rem` nRows, i `quot` nRows)
