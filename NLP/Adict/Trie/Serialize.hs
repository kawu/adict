{-# LANGUAGE BangPatterns #-}

module NLP.Adict.Trie.Serialize
( serialize
, deserialize
, implicitDAWG
) where

import Data.List (foldl')
import qualified Data.Map as M

import NLP.Adict.Trie
import NLP.Adict.Node

-- | Elminate common subtries.  The result is algebraically a trie
-- but is represented as a DAWG in memory.
implicitDAWG :: (Ord a, Ord b) => Trie a b -> Trie a b
implicitDAWG = deserialize . serialize

-- | Serialize the trie and eliminate all common subtries
-- along the way.
serialize :: (Ord a, Ord b) => Trie a b -> [Node a b]
serialize r =
    [ mkNode (valueIn t)
        [ (c, m M.! s)
        | (c, s) <- anyChild t ]
    | t <- M.elems m' ]
  where
    m  = collect r
    m' = M.fromList [(y, x) | (x, y) <- M.toList m]

-- | Construct the trie from the node list.
deserialize :: Ord a => [Node a b] -> Trie a b
deserialize =
    snd . M.findMax . foldl' update M.empty
  where
    update m n =
        let t = mkTrie (nodeValue n) [(c, m M.! k) | (c, k) <- nodeEdges n]
        in  M.insert (M.size m) t m

-- | Collect unique tries and assign identifiers to them.
collect :: (Ord a, Ord b) => Trie a b -> M.Map (Trie a b) Int
collect t = collect' M.empty t

collect' :: (Ord a, Ord b) => M.Map (Trie a b) Int
         -> Trie a b -> M.Map (Trie a b) Int
collect' m0 t = M.alter f t m
  where
    !m = foldl' collect' m0 (M.elems $ edgeMap t)
    !k = M.size m
    f Nothing  = Just k
    f (Just x) = Just x
