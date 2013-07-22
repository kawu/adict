module NLP.Adict.Basic
( findAll
) where

import           Data.Ix (range)
import qualified Data.Array as A
import qualified Data.Vector as V
import           Data.Vector.Unboxed (Unbox)

import           NLP.Adict.Core
import           Data.DAWG.Static (DAWG)
import qualified Data.DAWG.Static as D

-- | Find all words within a DAWG with restricted generalized edit distance
-- lower than or equal to a given threshold.
findAll
    :: (Enum a, Unbox w) 
    => Cost a               -- ^ Cost function
    -> Double               -- ^ Threshold
    -> Word a               -- ^ Query word
    -> DAWG a w b
    -> [([a], b, Double)]
findAll cost k x dawg =
    foundHere ++ foundLower
  where
    foundHere
        | dist' m <= k = case D.lookup [] dawg of
            Just y  -> [([], y, dist' m)]
            Nothing -> []
        | otherwise = []
    foundLower
        | minimum (A.elems distV) > k = []
        | otherwise = concatMap searchLower $ D.edges dawg
    searchLower = search cost k dist' x

    dist' = (A.!) distV 
    distV = A.array bounds [(i, dist i) | i <- range bounds]
    bounds = (0, m)
    m = V.length x

    dist 0 = 0
    dist i = dist' (i-1) + (delete cost) i (x#i)

search
    :: (Enum a, Unbox w)
    => Cost a               -- ^ Cost function
    -> Double               -- ^ Threshold
    -> (Int -> Double)      -- ^ ???
    -> Word a               -- ^ Query word
    -> (a, DAWG a w b)      -- ^ DAWG edge considered
    -> [([a], b, Double)]
search cost k distP x (c, dawg) =
    foundHere ++ map appendChar foundLower
  where
    foundHere
        | dist' m <= k = case D.lookup [] dawg of
            Just y  -> [([c], y, dist' m)]
            Nothing -> []
        | otherwise = []
    foundLower
        | minimum (A.elems distV) > k = []
        | otherwise = concatMap searchLower $ D.edges dawg
    searchLower = search cost k dist' x
    appendChar (cs, y, w) = ((c:cs), y, w)

    dist' = (A.!) distV 
    distV = A.array bounds [(i, dist i) | i <- range bounds]
    bounds  = (0, m)
    m = V.length x

    dist 0 = distP 0  + (insert cost) 0       c
    dist i = minimum
        [ distP (i-1) + (subst cost)  i (x#i) c
        , dist' (i-1) + (delete cost) i (x#i) 
        , distP i     + (insert cost) i       c ]
