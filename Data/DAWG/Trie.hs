{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.DAWG.Trie
( DAWG (..)
, mkDAWG
, serialize
, deserialize
, collect
) where

import Data.List (foldl')
import Control.Applicative ((<$>))
import Data.Binary (Binary, get, put)
import qualified Data.Map as M

import qualified Data.Trie.Class as C
import Data.Trie.Trie
import Data.DAWG.Node

newtype DAWG a = DAWG { unDAWG :: Trie a }
    deriving (Eq, Ord, C.Trie)

instance (Ord a, Binary a) => Binary (DAWG a) where
    put = put . serialize
    get = deserialize <$> get

mkDAWG :: Ord a => Trie a -> DAWG a
mkDAWG = deserialize . serialize . DAWG

serialize :: Ord a => DAWG a -> [Node a]
serialize (DAWG t) = 
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
