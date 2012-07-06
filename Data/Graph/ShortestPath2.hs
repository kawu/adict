{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Graph.ShortestPath2
( minPath
) where

import Control.Monad (when)
import Data.Function (on)
import Data.List (foldl')
import Data.PQueue.Min hiding (filter)

-- | Adjacent list for a given node @n. We assume, that it
-- is given in an ascending order.
type Edges n w = n -> [(n, w)]

-- | Is @n node an ending node?
type IsEnd n = n -> Bool

-- | Non-empty list of adjacent nodes given in ascending order.
-- We use newtype to implement custom Eq and Ord instances.
newtype Adj n w = Adj { unAdj :: [(n, w)] }

{-# INLINE proxy #-}
proxy :: Adj n w -> (n, w)
proxy = head . unAdj

{-# INLINE folls #-}
folls :: Adj n w -> [(n, w)]
folls = tail . unAdj

instance Eq w => Eq (Adj n w) where
    (==) = (==) `on` snd . proxy

instance Ord w => Ord (Adj n w) where
    compare = compare `on` snd . proxy

-- | Find shortes path from a beginning node to any ending node.
minPath :: (Ord n, Ord w, Num w) => w
        -> Edges n w -> IsEnd n -> n
        -> Maybe (n, w)
minPath threshold edgesFrom isEnd n =
    shortest $ singleton (Adj [(n, 0)])
  where
    -- | FIXME: Add visited set !!!
    shortest q = do
        (adj, q') <- minView q
        let (n, w) = proxy adj
        if isEnd n
            then Just (n, w)
            else shortest $ push (push q' $ folls adj) $
                    filter ((<= threshold) . snd)
                    [(m, w + v) | (m, v) <- edgesFrom n]
    push q [] = q
    push q xs = insert (Adj xs) q
