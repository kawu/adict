{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NLP.Adicts.Trie.DAWG
( DAWG (unDAWG)
, mkDAWG
, serialize
, deserialize
, collect
) where

import Data.List (foldl')
import Control.Applicative ((<$>))
import Data.Binary (Binary, get, put)
import qualified Data.Map as M

import qualified NLP.Adicts.Trie.Class as C
import NLP.Adicts.Trie
import NLP.Adicts.DAWG.Node

newtype DAWG a b = DAWG { unDAWG :: Trie a b }
    deriving (Eq, Ord, C.Trie)

-- instance Eq a => C.Trie (DAWG a b) where
--     unTrie      (DAWG t) = unTrie t
--     valueIn     (DAWG t) = valueIn t
--     anyChild    (DAWG t) = anyChild t
--     child x     (DAWG t) = child x t

instance (Ord a, Ord b, Binary a, Binary b) => Binary (DAWG a b) where
    put = put . serialize
    get = deserialize <$> get

mkDAWG :: Ord a => Trie a -> DAWG a
mkDAWG = deserialize . serialize . DAWG

serialize :: (Ord a, Ord b) => DAWG a b -> [Node a b]
serialize (DAWG t) = 
    [ mkNode (valueIn t)
        [ (c, m M.! s)
        | (c, s) <- C.anyChild t ]
    | t <- M.elems $ reverseMap m ]
  where
    m = collect t
    reverseMap m = M.fromList [(y, x) | (x, y) <- M.toList m]

-- | FIXME: Null node list case.
deserialize :: (Ord a, Ord b) => [Node a b] -> DAWG a b
deserialize =
    DAWG . snd . M.findMax . foldl' update M.empty
  where
    update m n =
        let t = C.mkTrie (nodeValue n) [(c, m M.! k) | (c, k) <- nodeEdges n]
        in  M.insert (M.size m) t m

-- | Collect unique tries and assign identifiers to them.
collect :: (Ord a, Ord b) => Trie a b -> M.Map (Trie a b) Int
collect t = collect' M.empty t

collect' :: (Ord a, Ord b) => M.Map (Trie a b) Int
         -> Trie a -> M.Map (Trie a b) Int
collect' m0 t = M.alter f t m
  where
    !m = foldl' collect' m0 (M.elems $ edgeMap t)
    !k = M.size m
    f Nothing  = Just k
    f (Just x) = Just x
