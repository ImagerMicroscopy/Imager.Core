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
rotateCW (v, (nRows, nCols)) = (rearrangeImage rotateCWIndex v (nRows, nCols), (nCols, nRows))

rotateCCW :: ImageProcessingFunc
rotateCCW (v, (nRows, nCols)) = (rearrangeImage rotateCCWIndex v (nRows, nCols), (nCols, nRows))

rearrangeImage :: CoordinateRearrangementFunc -> Vector Word16 -> (Int, Int) -> Vector Word16
rearrangeImage f v (nRows, nCols) = V.create $
    MV.new (nRows * nCols) >>= \mVec ->
    forM_ [0 .. (nRows * nCols - 1)] (\idx ->
        let newIndex = (rowColToLinear nCols . f (nRows, nCols) . linearToRowCol nRows) idx
        in  MV.write mVec newIndex (v V.! idx)) >>
    return mVec

rotateCWIndex :: CoordinateRearrangementFunc
rotateCWIndex (nRows, nCols) (row, col) = (nCols - 1 - col, row)

rotateCCWIndex  :: CoordinateRearrangementFunc
rotateCCWIndex (nRows, nCols) (row, col) = (col, nRows - 1 - row)

rowColToLinear :: Int -> (Int, Int) -> Int
rowColToLinear !nRows (!row, !col) = col * nRows + row

linearToRowCol :: Int -> Int -> (Int, Int)
linearToRowCol !nRows !i = (i `rem` nRows, i `quot` nRows)
