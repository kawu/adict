{-# LANGUAGE BangPatterns #-}

module Data.Adict.Fast
( search
) where

import Control.Monad (guard)
import Data.Maybe (catMaybes, maybeToList)

import Data.Trie.Class hiding (insert)
import Data.Adict.Base

type CostRow = [(Pos, Double)]
type Thres   = Double

-- | Find all words within a trie with restricted generalized edit distance
-- lower or equall to k.
search :: Trie t => Cost Char -> Thres -> Word
       -> t (Maybe a) -> [(Entry a, Double)]
search cost th x trie =
    foundHere ++ foundLower
  where
    foundHere = maybeToList $ do
        (k, v) <- maybeLast dist
        guard (k == m)
        x <- valueIn trie
        return (Entry [] x, v)
    foundLower
        | null dist = []
        | otherwise = concatMap searchLower $ anyChild trie
    searchLower = search' cost th 1 dist (x, m)
    m = wordSize x
    dist = mapDel cost th (x, m) 0 [(0, 0)]

search' :: Trie t => Cost Char -> Thres -> Pos -> CostRow -> (Word, Pos)
        -> (Char, t (Maybe a)) -> [(Entry a, Double)]
search' cost th j distP (x, m) (c, trie) =
    foundHere ++ map (appendChar c) foundLower
  where
    foundHere = maybeToList $ do
        (k, v) <- maybeLast dist
        guard (k == m)
        x <- valueIn trie
        return (Entry [c] x, v)
    foundLower
        | null dist = []
        | otherwise = concatMap searchLower $ anyChild trie
    searchLower = search' cost th (j+1) dist (x, m)
    appendChar c (Entry cs x, v) = (Entry (c:cs) x, v)

    dist = mapDel cost th (x, m) j $ merge
        [x | x@(_, !v) <- map ins distP, v <= th]
        [x | x@(_, !v) <- map sub distP, v <= th]

    {-# INLINE ins #-}
    ins (!i, !v) =
        let !v' = v + (insert cost) i (j, c)
        in  (i, v')

    {-# INLINE sub #-}
    sub (!k, !v)
        | k < m  =
            let !i = k + 1
                !v' = v + (subst cost)  (i, x#i) (j, c)
            in  (i, v')
        | otherwise = (k, th + 1.0) -- ^ Is it faster than (i, th + 1.0)?

mapDel :: Cost Char -> Thres -> (Word, Pos) -> Pos
       -> CostRow -> CostRow
mapDel cost th (x, m) j = doIt
  where
    doIt [] = []
    doIt (x:xs) =
        let xs' = merge (x:xs) (del x)
        in  head xs' : doIt (tail xs')
    del (k, v)
        | i <= m && u <= th = [(i, u)]
        | otherwise = []
      where
        (i, u) = (k + 1, v + (delete cost) (i, x#i) j)

merge :: CostRow -> CostRow -> CostRow
merge xs [] = xs
merge [] ys = ys
merge xs@((i, v):xs') ys@((j, w):ys')
    | i == j =
            let !u = min v w
            in (i, u) : merge xs' ys'
    | i < j  = (i, v) : merge xs' ys
    | i > j  = (j, w) : merge xs  ys'

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs
