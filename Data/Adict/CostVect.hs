{-# LANGUAGE BangPatterns #-}

module Data.Adict.CostVect
( CostVect
, minCost
, initVect
, nextVect
, match
, matchMod
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard, when)
import Data.Function (on)
import Data.Maybe (maybeToList)
import Data.List (minimumBy)

import Data.Trie.Class (Trie, valueIn)
import Data.Adict.Base hiding (cost, thres, word, wordSize)
import qualified Data.Adict.Base as B

-- | Edit distance vector for some trie node.
type CostVect = [(Pos, Double)]

-- | Minimum value in the cost vector.
minCost :: CostVect -> (Pos, Double)
minCost = minimumBy (compare `on` snd)

initVect :: Adict CostVect
initVect = mapDel 0 [(0, 0)]

nextVect :: Pos -> Char -> CostVect -> Adict CostVect
nextVect j c costVect = do
    cost <- B.cost
    th   <- B.thres
    word <- B.word
    m    <- B.wordSize
    doWith cost th word m
  where
    doWith cost th x m = mapDel j $ merge
        [x | x@(_, !v) <- map ins costVect, v <= th]
        [x | x@(_, !v) <- map sub costVect, v <= th]
      where
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

mapDel :: Pos -> CostVect -> Adict CostVect
mapDel j xs =
    doWith <$> B.cost <*> B.thres <*> B.word <*> B.wordSize
  where
    doWith cost th x m = doIt xs
      where
        doIt []     = []
        doIt (x:xs) =
            let xs' = merge (x:xs) (del x)
            in  head xs' : doIt (tail xs')
        {-# INLINE del #-}
        del (k, v)
            | i <= m && u <= th = [(i, u)]
            | otherwise = []
          where
            (i, u) = (k + 1, v + (delete cost) (i, x#i) j)

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
match :: Trie t => t (Maybe a) -> CostVect
      -> String -> Adict [(Entry a, Double)]
match trie costVect path =
    doWith <$> B.word <*> B.wordSize
  where
    doWith x m = maybeToList $ do
        v <- valueIn trie
        (k, dist) <- maybeLast costVect
        guard (k == m)
        return (Entry path v, dist)

matchMod :: Trie t => ThresMod -> t (Maybe a) -> CostVect
         -> String -> Adict [(Entry a, Double)]
matchMod thMod trie costVect path = do
    xs <- match trie costVect path
    case xs of
        [(entry, dist)] -> do
            -- th <- B.thres
            -- let thNew = thMod th dist
            -- when (thNew < th) $ do
            --     tell $ "change threshold from " ++ show th
            --         ++ " to " ++ show thNew ++ "\n"
            B.modThres (\th -> thMod th dist)
        [] -> return ()
    return xs

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs
