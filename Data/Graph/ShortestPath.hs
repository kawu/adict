{-# LANGUAGE BangPatterns #-}

module Data.Graph.ShortestPath
( minPath
-- , Path (..)
) where

import Control.Monad (when)
import Data.Function (on)
import Data.List (foldl')
import Data.PSQueue

import Debug.Trace (trace)

-- | List of edges for a given node @n.
type Edges n w = n -> [(n, w)]

-- | Is @n node an ending node?
type IsEnd n = n -> Bool

-- data Path n w = Path
--     { path   :: [n]
--     , weight :: w }
--     deriving Show
-- 
-- instance Eq w => Eq (Path n w) where
--     (==) = (==) `on` weight
-- 
-- instance Ord w => Ord (Path n w) where
--     compare = compare `on` weight

-- {-# INLINE (.+.) #-}
-- (.+.) :: Num w => Path n w -> (n, w) -> Path n w
-- Path xs w .+. (x, v) = Path (x:xs) (w+v)

-- | Find shortes path from a beginning node to any ending node.
-- minPath :: (Ord n, Ord w, Num w) => w
--         -> Edges n w -> IsEnd n -> n
--         -> Maybe (Path n w)
minPath threshold edgesFrom isEnd n =
    shortest $ singleton n 0 -- (Path [] 0)
  where
    shortest q = do
        (n :-> w, q') <- trace ("q: " ++ show (size q)) (minView q)
        -- when (weight w > threshold)
        when (w > threshold)
             (error "minPath: weight > threshold")
        if isEnd n
            then Just w
            else shortest $ foldl' push q'
                    -- [ (m, w .+. (m, v))
                    [ (m, w + v)
                    | (m, v) <- edgesFrom n ] 
    push q (n, w)
        -- | weight w > threshold = q
        | w > threshold = q
        | otherwise = insertWith min n w q
