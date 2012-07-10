module Data.Adict.Brute
( search
) where

import Data.Maybe (catMaybes)

import Data.Adict.Base
import Data.Adict.Dist

-- | Find all words within a list with restricted generalized edit distance
-- from x lower or equall to k.
search :: Cost -> Double -> Word -> [(Word, a)] -> [(Entry a, Double)]
search cost k x set =
    catMaybes $ map check set
  where
    check (y, v)
        | dist <= k = Just (Entry (toString y) v, dist)
        | otherwise = Nothing
      where
        dist = editDist cost x y
