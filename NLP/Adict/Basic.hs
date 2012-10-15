module NLP.Adict.Basic
( findAll
) where

import Data.Ix (range)
import qualified Data.Array as A
import qualified Data.Vector as V

import NLP.Adict.Core
import NLP.Adict.Trie.Internal hiding (insert)

-- | Find all words within a trie with restricted generalized edit distance
-- lower or equall to k.
findAll :: Cost a -> Double -> Word a -> TrieM a b -> [([a], b, Double)]
findAll cost k x trie =
    foundHere ++ foundLower
  where
    foundHere
        | dist' m <= k = case rootValue trie of
            Just y  -> [([], y, dist' m)]
            Nothing -> []
        | otherwise = []
    foundLower
        | minimum (A.elems distV) > k = []
        | otherwise = concatMap searchLower $ anyChild trie
    searchLower = search cost k dist' x

    dist' = (A.!) distV 
    distV = A.array bounds [(i, dist i) | i <- range bounds]
    bounds = (0, m)
    m = V.length x

    dist 0 = 0
    dist i = dist' (i-1) + (delete cost) i (x#i)

search :: Cost a -> Double -> (Int -> Double)
       -> Word a -> (a, TrieM a b) -> [([a], b, Double)]
search cost k distP x (c, trie) =
    foundHere ++ map appendChar foundLower
  where
    foundHere
        | dist' m <= k = case rootValue trie of
            Just y  -> [([c], y, dist' m)]
            Nothing -> []
        | otherwise = []
    foundLower
        | minimum (A.elems distV) > k = []
        | otherwise = concatMap searchLower $ anyChild trie
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
