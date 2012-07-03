module Data.Adict.Dist
( search
) where

import Data.Ix (range)
import qualified Data.Array as A

import Data.Adict.Base
import Data.Trie.Class hiding (insert)

-- | Find all words within a trie with restricted generalized edit distance
-- lower or equall to k.
search :: Trie t => Cost Char -> Double -> Word
       -> t (Maybe a) -> [(Entry a, Double)]
search cost k x trie =
    foundHere ++ foundLower
  where
    foundHere
        | dist' m <= k = case valueIn trie of
            Just x  -> [(Entry [] x, dist' m)]
            Nothing -> []
        | otherwise = []
    foundLower
        | minimum (A.elems distV) > k = []
        | otherwise = concatMap searchLower $ anyChild trie
    searchLower = search' cost k 1 dist' x

    dist' = (A.!) distV 
    distV = A.array bounds [(i, dist i) | i <- range bounds]
    bounds = (0, m)
    m = wordSize x

    dist 0 = 0
    dist i = dist' (i-1) + (delete cost) (i, x#i) 0

search' :: Trie t => Cost Char -> Double -> Int -> (Int -> Double)
        -> Word -> (Char, t (Maybe a)) -> [(Entry a, Double)]
search' cost k j distP x (c, trie) =
    foundHere ++ map (appendChar c) foundLower
  where
    foundHere
        | dist' m <= k = case valueIn trie of
            Just x  -> [(Entry [c] x, dist' m)]
            Nothing -> []
        | otherwise = []
    foundLower
        | minimum (A.elems distV) > k = []
        | otherwise = concatMap searchLower $ anyChild trie
    searchLower = search' cost k (j+1) dist' x
    appendChar c (Entry cs x, v) = (Entry (c:cs) x, v)

    dist' = (A.!) distV 
    distV = A.array bounds [(i, dist i) | i <- range bounds]
    bounds  = (0, m)
    m = wordSize x

    dist 0 = distP 0  + (insert cost)  0       (j, c)
    dist i = minimum
        [ distP (i-1) + (subst cost)  (i, x#i) (j, c)
        , dist' (i-1) + (delete cost) (i, x#i)  j
        , distP i     + (insert cost)  i       (j, c) ]