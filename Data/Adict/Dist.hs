module Data.Adict.Dist
( editDist
) where

import qualified Data.Array as A
import Data.Ix (range)

import Data.Adict.Base

-- | Restricted generalized edit distance between two words with
-- given cost function.
editDist :: Cost Char -> Word -> Word -> Double
editDist cost x y =
    dist' m n
  where
    dist' i j = distA A.! (i, j)
    distA = A.array bounds [(k, uncurry dist k) | k <- range bounds]
    bounds  = ((0, 0), (m, n))
    m = wordLength x
    n = wordLength y

    dist 0 0 = 0
    dist i 0 = dist' (i-1) 0 + (delete cost) (i, x#i)  0
    dist 0 j = dist' 0 (j-1) + (insert cost)  0       (j, y#j)
    dist i j = minimum
        [ dist' (i-1) (j-1)  + (subst cost)  (i, x#i) (j, y#j)
        , dist' (i-1) j      + (delete cost) (i, x#i)  j
        , dist' i (j-1)      + (insert cost)  i       (j, y#j) ]
