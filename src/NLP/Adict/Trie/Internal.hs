{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

-- | A (prefix) trie.

module NLP.Adict.Trie.Internal
( TrieM
, Trie (..)
, empty
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
, implicitDAWG
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>=>))
import Data.List (foldl')
import Data.Binary (Binary, get, put)
import qualified Data.Map as M

import NLP.Adict.Node

-- | A 'Trie' with 'Maybe' values in nodes.
type TrieM a b = Trie a (Maybe b)

-- | A trie of words with character type @a@ and entry type @b@.
-- It represents a 'M.Map' from @[a]@ keys to @b@ values.
data Trie a b = Trie {
    -- | Value in the root node.
    rootValue   :: b,                  
    -- | Edges to subtries annotated with characters.
    edgeMap     :: M.Map a (Trie a b)
    } deriving (Show, Eq, Ord)

instance Functor (Trie a) where
    fmap f Trie{..} = Trie (f rootValue) (fmap (fmap f) edgeMap)

instance (Ord a, Binary a, Binary b) => Binary (Trie a b) where
    put Trie{..} = do
        put rootValue
        put edgeMap
    get = Trie <$> get <*> get

-- | Decompose the trie into a pair of root value and edge list.
unTrie :: Trie a b -> (b, [(a, Trie a b)])
unTrie t = (rootValue t, M.toList $ edgeMap t)
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

-- | Empty trie.
empty :: Ord a => TrieM a b
empty = mkTrie Nothing []
{-# INLINE empty #-}

-- | Set the value in the root of the trie.
setValue :: b -> Trie a b -> Trie a b
setValue !x !t = t { rootValue = x }
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
insert :: Ord a => [a] -> b -> TrieM a b -> TrieM a b
insert [] v t = setValue (Just v) t
insert (x:xs) v t = substChild x t . insert xs v $
    case child x t of
        Just t' -> t'
        Nothing -> empty
{-# INLINABLE insert #-}
{-# SPECIALIZE insert
    :: String -> b
    -> TrieM Char b
    -> TrieM Char b #-}

-- | Size of the trie.
size :: Trie a b -> Int
size t = 1 + sum (map (size.snd) (anyChild t))

-- | Follow the path determined by the input word starting
-- in the trie root.
follow :: Ord a => [a] -> Trie a b -> Maybe (Trie a b)
follow xs t = foldr (>=>) return (map child xs) t

-- | Search for the value assigned to the given word in the trie.
lookup :: Ord a => [a] -> TrieM a b -> Maybe b
lookup xs t = follow xs t >>= rootValue

-- | Construct the trie from the list of (word, value) pairs.
fromList :: Ord a => [([a], b)] -> TrieM a b
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs

-- | Deconstruct the trie into a list of (word, value) pairs.
toList :: TrieM a b -> [([a], b)]
toList t = case rootValue t of
    Just y -> ([], y) : lower
    Nothing -> lower
  where
    lower = concatMap goDown $ anyChild t
    goDown (x, t') = map (addChar x) $ toList t'
    addChar x (xs, y) = (x:xs, y)

-- | Make the trie from the list of words.  Annotate each word with
-- the @()@ value.
fromLang :: Ord a => [[a]] -> TrieM a ()
fromLang xs = fromList [(x, ()) | x <- xs]

-- | Elminate common subtries.  The result is algebraically a trie
-- but is represented as a DAWG in memory.
implicitDAWG :: (Ord a, Ord b) => Trie a b -> Trie a b
implicitDAWG = deserialize . serialize

-- | Serialize the trie and eliminate all common subtries
-- along the way.
serialize :: (Ord a, Ord b) => Trie a b -> [Node a b]
serialize r =
    [ mkNode (rootValue t)
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
