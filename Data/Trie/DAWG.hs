{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Trie.DAWG
( DAWG (..)
, mkDAWG
, serialize
, deserialize
, collect
) where

import Data.List (foldl')
import Control.Applicative ((<$>))
import Data.Binary (Binary, encode, decode, get, put)
import qualified Data.Map as M

import qualified Data.Trie.Class as C
import Data.Trie

-- | FIXME: For DAWG there should be another Trie-like class
-- without operations which modify Trie structure (like mkTrie).
newtype DAWG a = DAWG { unDAWG :: Trie a }
    deriving (Eq, Ord, C.Trie)

mkDAWG :: Ord a => Trie a -> DAWG a
mkDAWG = deserialize . serialize

instance (Ord a, Binary a) => Binary (DAWG a) where
    put (DAWG t) = put (serialize t)
    get = deserialize <$> get

newtype Node a = Node { unNode :: (a, [(Char, Int)]) }
    deriving (Show, Eq, Ord, Binary)

mkNode :: a -> [(Char, Int)] -> Node a
mkNode x xs = Node (x, xs)

nodeValue :: Node a -> a
nodeValue = fst . unNode

nodeEdges :: Node a -> [(Char, Int)]
nodeEdges = snd . unNode

serialize :: Ord a => Trie a -> [Node a]
serialize t = 
    [ mkNode (valueIn t)
        [ (c, m M.! s)
        | (c, s) <- C.anyChild t ]
    | t <- M.elems $ reverseMap m ]
  where
    m = collect t
    reverseMap m = M.fromList [(y, x) | (x, y) <- M.toList m]

-- | FIXME: Null node list case.
deserialize :: Ord a => [Node a] -> DAWG a
deserialize =
    DAWG . snd . M.findMax . foldl' update M.empty
  where
    update m n =
        let t = C.mkTrie (nodeValue n) [(c, m M.! k) | (c, k) <- nodeEdges n]
        in  M.insert (M.size m) t m

-- | Collect unique tries and give them identifiers.
collect :: Ord a => Trie a -> M.Map (Trie a) Int
collect t = collect' M.empty t

collect' :: Ord a => M.Map (Trie a) Int -> Trie a -> M.Map (Trie a) Int
collect' m0 t = M.alter f t m
  where
    !m = foldl' collect' m0 (M.elems $ edgeMap t)
    !k = M.size m
    f Nothing  = Just k
    f (Just x) = Just x
