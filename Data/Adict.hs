module Data.Adict
( levenDist
, levenSearch
, defaultCost
) where

import Data.Ix (range)
import qualified Data.Array as A
import qualified Data.Text as T

import Data.Trie

type Cost = Char -> Char -> Double

-- | Levenshtein distance between two strings with given cost function.
levenDist :: Cost -> T.Text -> T.Text -> Double
levenDist cost xs ys = distMem m n where
    distArr = A.array bounds [(x, uncurry dist x) | x <- range bounds]
    bounds  = ((0, 0), (m, n))

    distMem i j
        | i >= 0 && j >= 0 = distArr A.! (i, j)
        | i < 0  && j < 0  = 0
        | otherwise        = 1 

    dist i j = minimum
        [ distMem (i-1) (j-1) + cost (T.index xs i) (T.index ys j)
        , distMem (i-1) j     + 1
        , distMem i (j-1)     + 1 ]

    m = T.length xs - 1
    n = T.length ys - 1

-- | Simple cost function: difference costs 1.
defaultCost :: Cost
defaultCost x y
    | x == y    = 0
    | otherwise = 1

-- | Find all words in a trie with Levenshtein distance lower or equall to k.
levenSearch :: Cost -> Double -> T.Text -> Trie a -> [(String, a)]
levenSearch cost k p trie =
    foundHere ++ foundLower
  where
    foundHere
        | fromIntegral (T.length p) <= k = case valueIn trie of
            Just x  -> [("", x)]
            Nothing -> []
        | otherwise = []
    foundLower = concatMap searchLower $ anyChild trie
    searchLower = doSearch cost k 1 distInit p
    distInit = fromIntegral . (+1)

-- | FIXME: Empty pattern case.
doSearch :: Cost -> Double -> Int -> (Int -> Double)
         -> T.Text -> (Char, Trie a) -> [(String, a)]
doSearch cost k depth distPar p (c, trie) = 
    foundHere ++ map (appendChar c) foundLower
  where
    distArr = A.array bounds [(x, dist x) | x <- range bounds]
    bounds  = (0, m)

    distMem (-1) = fromIntegral depth
    distMem i    = distArr A.! i

    dist i = minimum
        [ distPar (i-1) + cost (T.index p i) c
        , distPar i     + 1
        , distMem (i-1) + 1 ]

    foundHere
        | distMem m <= k = case valueIn trie of
            Just x  -> [(c:"", x)]
            Nothing -> []
        | otherwise = []
    foundLower
        | minimum (A.elems distArr) > k = []
        | otherwise = concatMap searchLower $ anyChild trie
      where
        searchLower = doSearch cost k (depth+1) distMem p
    appendChar c (cs, x) = (c:cs, x)

    m = T.length p - 1
