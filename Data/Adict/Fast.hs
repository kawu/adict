{-# LANGUAGE BangPatterns #-}

module Data.Adict.Fast
( search
) where

import Data.Trie.Class hiding (insert)
import Data.Adict.Base
import Data.Adict.CostVect

-- | Find all words within a trie with restricted generalized edit distance
-- lower or equall to k.
search :: Trie t => Cost Char -> Thres -> Word
       -> t (Maybe a) -> [(Entry a, Double)]
search cost th x trie =
    here ++ lower
  where
    here = match x trie dist []
    lower
        | null dist = []
        | otherwise = concatMap searchLower $ anyChild trie
    searchLower = search' cost th 1 dist x
    dist = initVect cost th x

search' :: Trie t => Cost Char -> Thres -> Pos -> CostVect -> Word
        -> (Char, t (Maybe a)) -> [(Entry a, Double)]
search' cost th j distP x (c, trie) =
    here ++ map (appendChar c) lower
  where
    here = match x trie dist [c]
    lower
        | null dist = []
        | otherwise = concatMap searchLower $ anyChild trie
    searchLower = search' cost th (j+1) dist x
    appendChar c (Entry cs x, v) = (Entry (c:cs) x, v)
    dist = nextVect cost th x j c distP
