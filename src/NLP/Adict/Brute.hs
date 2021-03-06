module NLP.Adict.Brute
( bruteSearch
) where

import Data.Maybe (mapMaybe)

import NLP.Adict.Core
import NLP.Adict.Dist

-- | Find all words within a list with restricted generalized edit distance
-- from x lower or equall to k.
bruteSearch :: Cost a -> Double -> Word a
            -> [(Word a, b)] -> [(Word a, b, Double)]
bruteSearch cost k x =
    mapMaybe check
  where
    check (y, v)
        | dist <= k = Just (y, v, dist)
        | otherwise = Nothing
      where
        dist = editDist cost x y
