{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}

module NLP.Adict.Graph
( minPath
, Edges
, IsEnd
) where

import Data.Function (on)
import qualified Data.PQueue.Min as P
import qualified Data.Map as M

-- | Adjacent list for a given node @n. We assume, that the list
-- is given in an ascending order.
type Edges n w = n -> [(w, n)]
type Edge n w  = (n, w, n)

-- | Is @n node an ending node?
type IsEnd n = n -> Bool

-- | Non-empty list of adjacent nodes given in ascending order.
-- We use new data type to implement custom Eq and Ord instances.
data Adj n w = Adj
    { from :: n
    , to   :: [(w, n)] }
    deriving Show

proxy :: Adj n w -> (w, n)
proxy = head . to
{-# INLINE proxy #-}

folls :: Adj n w -> [(w, n)]
folls = tail . to
{-# INLINE folls #-}

instance (Eq n, Eq w) => Eq (Adj n w) where
    (==) = (==) `on` proxy

instance (Ord n, Ord w) => Ord (Adj n w) where
    compare = compare `on` proxy

-- | Remove minimal edge (from, weight, to) from the queue.
minView :: (Ord n, Ord w) => P.MinQueue (Adj n w)
        -> Maybe (Edge n w, P.MinQueue (Adj n w))
minView queue = do
    (adj, queue') <- P.minView queue
    let p       = from adj
        (w, q)  = proxy adj
        e       = (p, w, q)
    return (e, push queue' p (folls adj))

push :: (Ord n, Ord w) => P.MinQueue (Adj n w) -> n
     -> [(w, n)] -> P.MinQueue (Adj n w)
push queue _ [] = queue
push queue p xs = P.insert (Adj p xs) queue

-- | Find shortes path from a beginning node to any ending node.
minPath :: (Ord n, Ord w, Num w, Fractional w)
        => w -> Edges n w -> IsEnd n -> n -> Maybe ([n], w)
minPath threshold edgesFrom isEnd beg =

    shortest M.empty $ P.singleton (Adj beg [(0, beg)])

  where

    -- | @visited -- set of visited nodes.
    --   @queue -- priority queue,
    shortest visited queue = do
        (edge, queue') <- minView queue
        shortest' visited queue' edge

    shortest' visited queue (p, w, q)
        | isEnd q               = Just (reverse (trace visited' q), w)
        | q `M.member` visited  = shortest visited  queue
        | otherwise             = shortest visited' queue'
      where
        visited' = M.insert q p visited
        queue' = push queue q $
                takeWhile ((<= threshold) . fst)
                [(w + u, s) | (u, s) <- edgesFrom q]

    trace visited n
        | m == n    = [n]
        | otherwise = n : trace visited m
      where
        m = visited M.! n
