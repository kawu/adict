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
search :: Trie t => t (Maybe a) -> Adict [(Entry a, Double)]
search trie = do
    dist <- initVect
    lower <- concatMapM (search' 1 dist) (anyChild trie)
    (++) <$> match trie dist [] <*> pure lower

search' :: Trie t => Pos -> CostVect -> (Char, t (Maybe a))
        -> Adict [(Entry a, Double)]
search' j distP (c, trie) = do
    dist <- nextVect j c distP
    here <- match trie dist [c]
    lower <- if null dist
        then return []
        else concatMapM (search' (j+1) dist) (anyChild trie)
    let appendChar c (Entry cs x, v) = (Entry (c:cs) x, v)
    return (here ++ map (appendChar c) lower)

concatMapM :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs
