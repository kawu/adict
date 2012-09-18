module NLP.Adicts.Brute
( search
) where

import Data.Maybe (mapMaybe)

import NLP.Adicts.Core
import NLP.Adicts.Dist

-- | Find all words within a list with restricted generalized edit distance
-- from x lower or equall to k.
search :: Cost a -> Double -> Word a -> [(Word a, b)] -> [(Word a, b, Double)]
search cost k x =
    mapMaybe check
  where
    check (y, v)
        | dist <= k = Just (y, v, dist)
        | otherwise = Nothing
      where
        dist = editDist cost x y
