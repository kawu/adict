{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Data.Trie.MapTrie
( Trie (..)
, size
, listTrie
) where

import Data.List (foldl')
import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>), Sum (..))
import Data.Foldable (Foldable, foldMap)
import Data.Binary (Binary, encode, decode, get, put)
import qualified Data.Map as M

import qualified Data.Trie.Generic as G

data Trie a = Trie
    { valueIn :: a
    , edgeMap :: M.Map Char (Trie a) }
    deriving (Eq, Ord)

instance Functor Trie where
    fmap f Trie{..} = Trie (f valueIn) (fmap (fmap f) edgeMap)

instance Foldable Trie where
    foldMap f Trie{..} = foldMap (foldMap f) edgeMap <> f valueIn

instance G.Trie Trie where
    mkTrie !v !cs = Trie v (M.fromList cs)
    unTrie t    = (valueIn t, M.toList $ edgeMap t)
    child x Trie{..} = x `M.lookup` edgeMap
    setValue !x !t = t { valueIn = x }
    subChild !x !trie !newChild =
        let how _ = Just newChild
            !edges = M.alter how x (edgeMap trie)
        in trie { edgeMap = edges }

instance Show a => Show (Trie (Maybe a)) where
    show = show . G.toList

instance Binary a => Binary (Trie a) where
    put Trie{..} = do
        put valueIn
        put edgeMap
    get = Trie <$> get <*> get

size :: Trie a -> Int
size = getSum . foldMap (const $ Sum 1)

listTrie :: Trie a -> [Trie a]
listTrie t = t : concatMap listTrie (M.elems $ edgeMap t)
