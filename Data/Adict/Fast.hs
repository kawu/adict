module Data.Adict.Fast
( levenSearch
, module Data.RadixTree
) where

import Control.Monad (guard)
import Data.Maybe (catMaybes, maybeToList)
import Data.Ix (range)
import Data.ListLike (ListLike)
import qualified Data.ListLike as L
import qualified Data.Array as A

import Data.RadixTree
import Data.Adict hiding (levenSearch)

(!) :: ListLike full item => full -> Int -> item
(!) = L.index

(#) :: ListLike w a => w -> Int -> a
x#i = x!(i-1)

type CostRow = [(Pos, Double)]
type Thres   = Double

-- | Find all words within a trie with restricted generalized edit distance
-- lower or equall to k.
levenSearch :: (Eq a, ListLike w a) => Cost a -> Thres -> w
            -> Trie a b -> [(Entry a b, Double)]
levenSearch cost th x trie =
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
    searchLower = levenSearch' cost th 1 dist x
    m = L.length x
    dist = mapDel cost th x 0 [(0, 0)]

levenSearch' :: (Eq a, ListLike w a)
             => Cost a -> Thres -> Pos -> CostRow -> w
             -> (a, Trie a b) -> [(Entry a b, Double)]
levenSearch' cost th j distP x (c, trie) =
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
    searchLower = levenSearch' cost th (j+1) dist x
    appendChar c (Entry cs x, v) = (Entry (c:cs) x, v)
    m = L.length x  -- ^ FIXME: O(n) !

    dist = mapDel cost th x j $ merge (map' ins distP) (map' sub distP)
    ins (i, v) = (i, v + (insert cost)  i       (j, c))
    sub (k, v) = (i, v + (subst cost)  (i, x#i) (j, c)) where i = k+1
    map' f xs  = [x | x@(k, v) <- map f xs, k <= m, v <= th]

mapDel :: ListLike w a => Cost a -> Thres -> w -> Pos -> CostRow -> CostRow
mapDel cost th x j = doIt
  where
    doIt [] = []
    doIt (x:xs) =
        let xs' = merge (x:xs) (del x)
        in  head xs' : doIt (tail xs')
    del (i, v)
        | k <= m && u <= th = [(k, u)]
        | otherwise = []
      where
        (k, u) = (i + 1, v + (delete cost) (k, x#k) j)
    m = L.length x  -- ^ FIXME: O(n) !

merge :: CostRow -> CostRow -> CostRow
merge xs [] = xs
merge [] ys = ys
merge xs@((i, v):xs') ys@((j, w):ys')
    | i == j = (i, min v w) : merge xs' ys'
    | i < j  = (i, v)       : merge xs' ys
    | i > j  = (j, w)       : merge xs  ys'

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs
