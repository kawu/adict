{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}

module NLP.Adict.Graph
( minPath
, Edges
, IsEnd
) where

import qualified Data.PSQueue as P
import qualified Data.Map as M

-- | Adjacent list for a given node @n. We assume, that the list
-- is given in an ascending order.
type Edges n w = n -> [(w, n)]
type Edge n w  = (n, w, n)

-- | Is @n node an ending node?
type IsEnd n = n -> Bool

-- | Non-empty list of adjacent nodes given in an ascending order.
data Adj n w = Adj
    { from :: n
    , to   :: [(w, n)] }
    deriving (Show, Eq, Ord)

-- | First element from the the adjacent list, which is also
-- a priority in the priority queue.
proxy :: Adj n w -> (w, n)
proxy = head . to
{-# INLINE proxy #-}

-- | Tail elements from the adjacent list.
folls :: Adj n w -> [(w, n)]
folls = tail . to
{-# INLINE folls #-}

-- | Priority queue.
type PQ n w = P.PSQ (Adj n w) (w, n)

-- | Remove minimal edge (from, weight, to) from the queue.
minView :: (Ord n, Ord w) => PQ n w -> Maybe (Edge n w, PQ n w)
minView queue = do
    (adj P.:-> (w, q), queue') <- P.minView queue
    let p       = from adj
        e       = (p, w, q)
    return (e, push queue' p (folls adj))

push :: (Ord n, Ord w) => PQ n w -> n -> [(w, n)] -> PQ n w
push queue _ [] = queue
push queue p xs = insert (Adj p xs) queue
{-# INLINE push #-}

insert :: (Ord n, Ord w) => Adj n w -> PQ n w -> PQ n w
insert x = P.insert x (proxy x)
{-# INLINE insert #-}

-- | Find the shortest path from the beginning node to one
-- of the ending nodes.
minPath :: (Show n, Show w, Ord n, Ord w, Num w, Fractional w)
        => w -> Edges n w -> IsEnd n -> n -> Maybe ([n], w)
minPath threshold edgesFrom isEnd beg =

    shortest M.empty $ insert (Adj beg [(0, beg)]) P.empty

  where

    -- @visited: set of visited nodes
    -- @queue: priority queue
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
