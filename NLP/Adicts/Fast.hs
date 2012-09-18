{-# LANGUAGE BangPatterns #-}

module Data.Adict.Fast
( search
) where

import Control.Applicative (pure, (<$>), (<*>))

import Data.Trie.Class hiding (insert)
import Data.Adict.Base
import Data.Adict.CostVect

-- | Find all words within a trie with restricted generalized edit distance
-- lower or equall to k.
search :: Trie t => Cost -> Double -> Word
       -> t (Maybe a) -> [(Entry a, Double)]
search cost k x trie
    =  match x trie dist []
    ++ concatMap searchLower (anyChild trie)
  where
    dist = initVect cost k x
    searchLower = search' cost k x dist

search' :: Trie t => Cost -> Double -> Word -> CostVect
        -> (Char, t (Maybe a)) -> [(Entry a, Double)]
search' cost k x distP (c, trie) =
    here ++ map (appendChar c) lower
  where
    dist = nextVect cost k x c distP
    here = match x trie dist [c]
    lower = if null dist
        then []
        else concatMap searchLower (anyChild trie)
    searchLower = search' cost k x dist 
    appendChar c (Entry cs x, v) = (Entry (c:cs) x, v)
