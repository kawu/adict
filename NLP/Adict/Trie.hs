{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

-- | A (prefix) trie.

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
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>=>))
import Data.List (foldl')
import Data.Binary (Binary, get, put)
import qualified Data.Map as M

-- | A 'Trie' with 'Maybe' values in nodes.
type TrieD a b = Trie a (Maybe b)

-- | A trie of words with character type @a@ and entry type @b@.  It can be
-- thought of as a mapping from @[a]@s to @b@s.
data Trie a b = Trie {
    -- | Value in the node.
    valueIn :: b,                  
    -- | Edges to subtries annotated with characters.
    edgeMap :: M.Map a (Trie a b)
    } deriving (Show, Eq, Ord)

instance Functor (Trie a) where
    fmap f Trie{..} = Trie (f valueIn) (fmap (fmap f) edgeMap)

instance (Ord a, Binary a, Binary b) => Binary (Trie a b) where
    put Trie{..} = do
        put valueIn
        put edgeMap
    get = Trie <$> get <*> get

-- | Decompose the trie into a pair of root value and edge list.
unTrie :: Trie a b -> (b, [(a, Trie a b)])
unTrie t = (valueIn t, M.toList $ edgeMap t)
{-# INLINE unTrie #-}

-- | Child of the trie following the edge annotated with the given character.
child :: Ord a => a -> Trie a b -> Maybe (Trie a b)
child x Trie{..} = x `M.lookup` edgeMap
{-# INLINE child #-}

-- | Return trie edges as a list of (annotation character, subtrie) pairs.
anyChild :: Trie a b -> [(a, Trie a b)]
anyChild = snd . unTrie
{-# INLINE anyChild #-}

-- | Construct trie from the root value and the list of edges.
mkTrie :: Ord a => b -> [(a, Trie a b)] -> Trie a b
mkTrie !v !cs = Trie v (M.fromList cs)
{-# INLINE mkTrie #-}

-- | Empty 'TrieD'.
empty :: Ord a => TrieD a b
empty = mkTrie Nothing []
{-# INLINE empty #-}

-- | Set the value in the root of the trie.
setValue :: b -> Trie a b -> Trie a b
setValue !x !t = t { valueIn = x }
{-# INLINE setValue #-}

-- | Substitute subtrie attached to the edge annotated with the given
-- character (or add new edge if such edge did not exist).
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

-- | Insert word with the given value to the trie.
insert :: Ord a => [a] -> b -> TrieD a b -> TrieD a b
insert [] v t = setValue (Just v) t
insert (x:xs) v t = substChild x t . insert xs v $
    case child x t of
        Just t' -> t'
        Nothing -> empty
{-# INLINABLE insert #-}
{-# SPECIALIZE insert
    :: String -> b
    -> TrieD Char b
    -> TrieD Char b #-}

-- | Size of the trie.
size :: Trie a b -> Int
size t = 1 + sum (map (size.snd) (anyChild t))

-- | Follow the path determined by the input word starting
-- in the trie root.
follow :: Ord a => [a] -> Trie a b -> Maybe (Trie a b)
follow xs t = foldr (>=>) return (map child xs) t

-- | Search for the value assigned to the given word in the trie.
lookup :: Ord a => [a] -> TrieD a b -> Maybe b
lookup xs t = follow xs t >>= valueIn

-- | Construct the trie from the list of (word, value) pairs.
fromList :: Ord a => [([a], b)] -> TrieD a b
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs

-- | Deconstruct the trie into a list of (word, value) pairs.
toList :: TrieD a b -> [([a], b)]
toList t = case valueIn t of
    Just y -> ([], y) : lower
    Nothing -> lower
  where
    lower = concatMap goDown $ anyChild t
    goDown (x, t') = map (addChar x) $ toList t'
    addChar x (xs, y) = (x:xs, y)

-- | Make the trie from the list of words.  Annotate each word with
-- the @()@ value.
fromLang :: Ord a => [[a]] -> TrieD a ()
fromLang xs = fromList [(x, ()) | x <- xs]
