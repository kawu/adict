{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A graph node data type.

module NLP.Adict.Node
( Node
, mkNode
, nodeValue
, nodeEdges
) where

import Data.Binary (Binary)

newtype Node a b = Node { unNode :: (b, [(a, Int)]) }
    deriving (Show, Eq, Ord, Binary)

mkNode :: b -> [(a, Int)] -> Node a b
mkNode x xs = Node (x, xs)

nodeValue :: Node a b -> b
nodeValue = fst . unNode

nodeEdges :: Node a b -> [(a, Int)]
nodeEdges = snd . unNode
