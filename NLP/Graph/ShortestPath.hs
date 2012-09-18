{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Graph.ShortestPath
( minPath
) where

import Control.Monad (when)
import Data.Function (on)
import Data.List (foldl')
import qualified Data.Set as P
import qualified Data.Map as M

-- | Adjacent list for a given node @n. We assume, that it
-- is given in an ascending order.
type Edges n w = n -> [(n, w)]

-- | Is @n node an ending node?
type IsEnd n = n -> Bool

-- | Non-empty list of adjacent nodes given in ascending order.
-- We use new data type to implement custom Eq and Ord instances.
data Adj n w = Adj
    { parent :: n
    , unAdj  :: [(n, w)] }

{-# INLINE proxy #-}
proxy :: Adj n w -> (n, w)
proxy = head . unAdj

{-# INLINE folls #-}
folls :: Adj n w -> [(n, w)]
folls = tail . unAdj

{-# INLINE swap #-}
swap :: (a, b) -> (b, a)
swap ~(x, y) = (y, x)

instance (Eq n, Eq w) => Eq (Adj n w) where
    (==) = (==) `on` swap . proxy

instance (Ord n, Ord w) => Ord (Adj n w) where
    compare = compare `on` swap . proxy

-- | Find shortes path from a beginning node to any ending node.
minPath :: (Show n, Show w, Ord n, Ord w, Num w, Fractional w) => w
        -> Edges n w -> IsEnd n -> n
        -> Maybe ([n], w)
minPath threshold edgesFrom isEnd n =

    shortest M.empty $ P.singleton (Adj n [(n, 0)])

  where

    -- | @v -- set of visited nodes.
    --   @q -- priority queue,
    shortest v q = do
        (adj, q') <- P.minView q
        process q' adj
      where
        process q adj
            | isEnd n        = Just (reverse (trace v' n), w)
            | n `M.member` v = shortest v  q'
            | otherwise      = shortest v' q''
          where
            (n, w) = proxy adj
            pr = parent adj
            v' = M.insert n pr v
            q' = push q pr (folls adj)
            q'' = push q' n $
                    takeWhile ((<= threshold) . snd)
                    [(m, w + v) | (m, v) <- edgesFrom n]

    push q p [] = q
    push q p xs = P.insert (Adj p xs) q

    trace v n
        | m == n    = [n]
        | otherwise = n : trace v m
      where
        m = v M.! n
