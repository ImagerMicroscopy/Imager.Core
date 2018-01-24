module FilterUtils where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import MiscUtils

validateFilters :: (Int, Int) -> [(Text, Int)] -> [(Text, Int)]
validateFilters idxLimits chs
    | haveDuplicates chs = error ("duplicate channels in " ++ show chs)
    | invalidFilterIndices idxLimits chs = error ("invalid filter indices in "  ++ show chs)
    | invalidFilterNames chs = error ("invalid filter names in " ++ show chs)
    | otherwise = chs
    where
        haveDuplicates chs = not ((nodups (map fst chs)) && (nodups (map snd chs)))
        nodups xs = (nub xs) == xs
        invalidFilterIndices (minIdx, maxIdx) = any (\(_, i) -> not (within i minIdx maxIdx))
        invalidFilterNames = any (\(n, _) -> T.null n)
