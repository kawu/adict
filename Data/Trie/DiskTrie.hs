{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Trie.DiskTrie
(
) where

import Prelude hiding (mapM)
import System.IO (Handle, hSeek, SeekMode(..))
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Applicative ((<$>), (<*>))
import Data.Foldable
import Data.Traversable
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Binary (Binary, put, get, encode, decode, Get)

import qualified Data.Trie.Generic as G

-- | Pattern functor of a Trie based on Data.Map.
-- TODO: Try changing (Maybe a) to a.
data MapTrie a r = MapTrie
    { valueIn :: Maybe a
    , edgeMap :: M.Map Char r }

instance (Binary a, Binary r) => Binary (MapTrie a r) where
    put MapTrie{..} = do
        put valueIn
        put edgeMap
    get = MapTrie <$> get <*> get

-- | Pattern functor is indeed a functor.
instance Functor (MapTrie a) where
    fmap f MapTrie{..} = MapTrie valueIn (fmap f edgeMap)

instance Foldable (MapTrie a) where
    foldMap f MapTrie{..} = foldMap f edgeMap

instance Traversable (MapTrie a) where
    traverse f MapTrie{..} = MapTrie valueIn <$> traverse f edgeMap

-- | Trie for a given pattern.
newtype TrieOn t a = Trie (t a (TrieOn t a))

instance G.Trie (TrieOn MapTrie) where
    mkTrie v cs    = Trie $ MapTrie v (M.fromList cs)
    unTrie (Trie t)  = (valueIn t, M.toList $ edgeMap t)
    child x (Trie t) = x `M.lookup` edgeMap t
    setValue x (Trie t) = Trie $ t { valueIn = x }
    subChild x (Trie trie) newChild =
        let how _ = Just newChild
        in Trie $ trie { edgeMap = M.alter how x (edgeMap trie) }

type Trie = TrieOn MapTrie

instance Show a => Show (Trie a) where
    show = show . G.toList

data OnDisk t a
    = InMem (t a (OnDisk t a))
    | Offset Integer

instance Binary a => Binary (OnDisk MapTrie a) where
    put (InMem t) = do
        put (0 :: Int)
        put t
    put (Offset x) = do
        put (1 :: Int)
        put x
    get = do
        k <- get :: Get Int
        case k of
            0 -> InMem  <$> get 
            1 -> Offset <$> get

type DiskTrie = OnDisk MapTrie

fromDisk :: Binary a => Handle -> DiskTrie a -> IO (Trie a)
fromDisk h (InMem trie) = Trie <$> mapM (fromDisk h) trie
fromDisk h (Offset x) = unsafeInterleaveIO $ do
    hSeek h AbsoluteSeek x
    size <- decode <$> B.hGet h 4
    trie <- decode <$> B.hGet h size
    Trie <$> mapM (fromDisk h) trie
