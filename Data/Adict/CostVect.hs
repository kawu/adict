{-# LANGUAGE BangPatterns #-}

module Data.Adict.CostVect
( CostVect
, minCost
, initVect
, nextVect
) where

import Data.Function (on)
import Data.List (minimumBy)

import Data.Adict.Base

-- | Edit distance vector for some trie node.
type CostVect = [(Pos, Double)]

-- | Minimum value in the cost vector.
minCost :: CostVect -> (Pos, Double)
minCost = minimumBy (compare `on` snd)

initVect :: Cost Char -> Thres -> Word -> CostVect
initVect cost th x = mapDel cost th x 0 [(0, 0)]

nextVect :: Cost Char -> Thres -> Word -> Pos -> Char -> CostVect -> CostVect
nextVect cost th x j c costVect = mapDel cost th x j $ merge
    [x | x@(_, !v) <- map ins costVect, v <= th]
    [x | x@(_, !v) <- map sub costVect, v <= th]
  where
    m = wordSize x
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

mapDel :: Cost Char -> Thres -> Word -> Pos -> CostVect -> CostVect
mapDel cost th w j = doIt
  where
    doIt []     = []
    doIt (x:xs) =
        let xs' = merge (x:xs) (del x)
        in  head xs' : doIt (tail xs')
    m = wordSize w
    {-# INLINE del #-}
    del (k, v)
        | i <= m && u <= th = [(i, u)]
        | otherwise = []
      where
        (i, u) = (k + 1, v + (delete cost) (i, w#i) j)

merge :: CostVect -> CostVect -> CostVect
merge xs [] = xs
merge [] ys = ys
merge xs@((i, v):xs') ys@((j, w):ys')
    | i == j =
            let !u = min v w
            in (i, u) : merge xs' ys'
    | i < j  = (i, v) : merge xs' ys
    | i > j  = (j, w) : merge xs  ys'
