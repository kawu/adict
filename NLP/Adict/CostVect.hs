{-# LANGUAGE BangPatterns #-}

module Data.Adict.CostVect
( CostVect
, minCost
, initVect
, nextVect
, match
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard)
import Data.Function (on)
import Data.Maybe (maybeToList)
import Data.List (minimumBy)

import Data.Trie.Class (Trie, valueIn)
import Data.Adict.Base

-- | Edit distance vector for some trie node.
type CostVect = [(Pos, Double)]

-- | Minimum value in the cost vector.
minCost :: CostVect -> (Pos, Double)
minCost = minimumBy (compare `on` snd)

initVect :: Cost -> Double -> Word -> CostVect
initVect cost k x = mapDel cost k x [(0, 0)]

nextVect :: Cost -> Double -> Word -> Char -> CostVect -> CostVect
nextVect cost z x c costVect = mapDel cost z x $ merge
    [x | x@(_, !v) <- map ins costVect, v <= z]
    [x | x@(_, !v) <- map sub costVect, v <= z]
  where
    m = wordLength x
    {-# INLINE ins #-}
    ins (!i, !v) =
        let !v' = v + (insert cost) i c
        in  (i, v')
    {-# INLINE sub #-}
    sub (!k, !v)
        | k < m  =
            let !i = k + 1
                !v' = v + (subst cost) i (x#i) c
            in  (i, v')
        | otherwise = (k, z + 1.0) -- ^ Is it faster than (i, z + 1.0)?

mapDel :: Cost -> Double -> Word -> CostVect -> CostVect
mapDel cost z x xs =
    doIt xs
  where
    m = wordLength x
    doIt []     = []
    doIt (x:xs) =
        let xs' = merge (x:xs) (del x)
        in  head xs' : doIt (tail xs')
    {-# INLINE del #-}
    del (k, v)
        | i <= m && u <= z = [(i, u)]
        | otherwise = []
      where
        i = k + 1
        u = v + (delete cost) i (x#i)

merge :: CostVect -> CostVect -> CostVect
merge xs [] = xs
merge [] ys = ys
merge xs@((i, v):xs') ys@((j, w):ys')
    | i == j =
            let !u = min v w
            in (i, u) : merge xs' ys'
    | i < j  = (i, v) : merge xs' ys
    | i > j  = (j, w) : merge xs  ys'

-- | Return 1 element list with matching Entry, if entries edit
-- distance is not greater than threshold. Otherwise, return
-- empty list.
match :: Trie t => Word -> t (Maybe a) -> CostVect
      -> String -> [(Entry a, Double)]
match x trie costVect path = maybeToList $ do
    v <- valueIn trie
    (k, dist) <- maybeLast costVect
    guard (k == m)
    return (Entry path v, dist)
  where
    m = wordLength x

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs
