{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.DAWG.Node
( Node
, mkNode
, nodeValue
, nodeEdges
) where

import Data.Binary (Binary)

newtype Node a = Node { unNode :: (a, [(Char, Int)]) }
    deriving (Show, Eq, Ord, Binary)

mkNode :: a -> [(Char, Int)] -> Node a
mkNode x xs = Node (x, xs)

nodeValue :: Node a -> a
nodeValue = fst . unNode

nodeEdges :: Node a -> [(Char, Int)]
nodeEdges = snd . unNode
