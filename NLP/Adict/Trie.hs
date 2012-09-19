{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module NLP.Adict.Trie
( TrieD
, Trie (..)
, unTrie
, child
, anyChild
, mkTrie
, setValue
, substChild
, insert

, size
, follow
, lookup
, fromLang
, fromList
, toList

, serialize
, deserialize
, toDAWG
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>=>))
import Data.List (foldl')
import Data.Binary (Binary, get, put)
import qualified Data.Map as M

import NLP.Adict.DAWG.Node

type TrieD a b = Trie a (Maybe b)

data Trie a b = Trie
    { valueIn :: b
    , edgeMap :: M.Map a (Trie a b) }
    deriving (Show, Eq, Ord)

instance Functor (Trie a) where
    fmap f Trie{..} = Trie (f valueIn) (fmap (fmap f) edgeMap)

instance (Ord a, Binary a, Binary b) => Binary (Trie a b) where
    put Trie{..} = do
        put valueIn
        put edgeMap
    get = Trie <$> get <*> get

unTrie :: Trie a b -> (b, [(a, Trie a b)])
unTrie t = (valueIn t, M.toList $ edgeMap t)
{-# INLINE unTrie #-}

child :: Ord a => a -> Trie a b -> Maybe (Trie a b)
child x Trie{..} = x `M.lookup` edgeMap
{-# INLINE child #-}

anyChild :: Trie a b -> [(a, Trie a b)]
anyChild = snd . unTrie
{-# INLINE anyChild #-}

mkTrie :: Ord a => b -> [(a, Trie a b)] -> Trie a b
mkTrie !v !cs = Trie v (M.fromList cs)
{-# INLINE mkTrie #-}

empty :: Ord a => Trie a (Maybe b)
empty = mkTrie Nothing []
{-# INLINE empty #-}

setValue :: b -> Trie a b -> Trie a b
setValue !x !t = t { valueIn = x }
{-# INLINE setValue #-}

substChild :: Ord a => a -> Trie a b -> Trie a b -> Trie a b
substChild !x !trie !newChild =
    let how _ = Just newChild
        !edges = M.alter how x (edgeMap trie)
    in trie { edgeMap = edges }
{-# INLINABLE substChild #-}
{-# SPECIALIZE substChild
    :: Char
    -> Trie Char b
    -> Trie Char b
    -> Trie Char b #-}

insert :: Ord a => [a] -> b -> Trie a (Maybe b) -> Trie a (Maybe b)
insert [] v t = setValue (Just v) t
insert (x:xs) v t = substChild x t . insert xs v $
    case child x t of
        Just t' -> t'
        Nothing -> empty
{-# INLINABLE insert #-}
{-# SPECIALIZE insert
    :: String -> b
    -> Trie Char (Maybe b)
    -> Trie Char (Maybe b) #-}

size :: Trie a b -> Int
size t = 1 + sum (map (size.snd) (anyChild t))

follow :: Ord a => [a] -> Trie a b -> Maybe (Trie a b)
follow xs t = foldr (>=>) return (map child xs) t

lookup :: Ord a => [a] -> Trie a (Maybe b) -> Maybe b
lookup xs t = follow xs t >>= valueIn

fromList :: Ord a => [([a], b)] -> Trie a (Maybe b)
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs

toList :: Trie a (Maybe b) -> [([a], b)]
toList t = case valueIn t of
    Just y -> ([], y) : lower
    Nothing -> lower
  where
    lower = concatMap goDown $ anyChild t
    goDown (x, t') = map (addChar x) $ toList t'
    addChar x (xs, y) = (x:xs, y)

fromLang :: Ord a => [[a]] -> Trie a (Maybe ())
fromLang xs = fromList [(x, ()) | x <- xs]

toDAWG :: (Ord a, Ord b) => Trie a b -> Trie a b
toDAWG = deserialize . serialize

serialize :: (Ord a, Ord b) => Trie a b -> [Node a b]
serialize r =
    [ mkNode (valueIn t)
        [ (c, m M.! s)
        | (c, s) <- anyChild t ]
    | t <- M.elems m' ]
  where
    m  = collect r
    m' = M.fromList [(y, x) | (x, y) <- M.toList m]

-- | FIXME: Null node list case.
deserialize :: (Ord a, Ord b) => [Node a b] -> Trie a b
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
