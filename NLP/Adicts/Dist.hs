module NLP.Adicts.Dist
( editDist
) where

import qualified Data.Array as A
import qualified Data.Vector as V
import Data.Ix (range)

import NLP.Adicts.Core

-- | Restricted generalized edit distance between two words with
-- given cost function.
editDist :: Cost a -> Word a -> Word a -> Double
editDist cost x y =
    dist' m n
  where
    dist' i j = distA A.! (i, j)
    distA = A.array bounds [(k, uncurry dist k) | k <- range bounds]
    bounds  = ((0, 0), (m, n))
    m = V.length x
    n = V.length y

    dist 0 0 = 0
    dist i 0 = dist' (i-1) 0 + (delete cost) i (x#i)
    dist 0 j = dist' 0 (j-1) + (insert cost) 0       (y#j)
    dist i j = minimum
        [ dist' (i-1) (j-1)  + (subst cost)  i (x#i) (y#j)
        , dist' (i-1) j      + (delete cost) i (x#i) 
        , dist' i (j-1)      + (insert cost) i       (y#j) ]
