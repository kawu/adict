{-# LANGUAGE MultiParamTypeClasses #-}

module NLP.Adicts.Trie.Class
( Trie (..)
, TrieM (..)
, size
, follow
, lookup
, fromList
, toList
, fromLang
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Data.List (find, foldl')

-- | Generic Trie interface.  Only non-modifying operations.
-- Minimal complete definition: unTrie.
class Trie t where
    unTrie      :: t a b -> (b, [(a, t a b)])
    valueIn     :: t a b -> b
    anyChild    :: t a b -> [(a, t a b)]
    child       :: Eq a => a -> t a b -> Maybe (t a b)

    -- | Default implementations.
    valueIn     = fst . unTrie
    anyChild    = snd . unTrie
    child x t   = snd <$> find ((x==).fst) (anyChild t)

-- | Generic TrieM interface.  Minimal complete definition: mkTrie.
class Trie t => TrieM t where
    mkTrie      :: b -> [(a, t a b)] -> t a b
    empty       :: t a (Maybe b)
    setValue    :: b -> t a b -> t a b
    substChild  :: Eq a => a -> t a b -> t a b -> t a b
    insert      :: Eq a => [a] -> b -> t a (Maybe b) -> t a (Maybe b)
    fromTrie    :: Trie s => s a b -> t a b

    -- | Default implementations.
    empty       = mkTrie Nothing []
    setValue v  = mkTrie v . anyChild 
    substChild x t c =
        let cs = filter ((x/=).fst) (anyChild t)
        in  mkTrie (valueIn t) ((x, c):cs)
    insert [] v t = setValue (Just v) t
    insert (x:xs) v t = substChild x t . insert xs v $
        case child x t of
            Just t' -> t'
            Nothing -> empty
    fromTrie t = mkTrie (valueIn t)
        [ (x, fromTrie s)
        | (x, s) <- anyChild t ]

size :: Trie t => t a b -> Int
size t = 1 + sum (map (size.snd) (anyChild t))

follow :: (Eq a, Trie t) => [a] -> t a b -> Maybe (t a b)
follow xs t = foldr (>=>) return (map child xs) t

lookup :: (Eq a, Trie t) => [a] -> t a (Maybe b) -> Maybe b
lookup xs t = follow xs t >>= valueIn

fromList :: (Eq a, TrieM t) => [([a], b)] -> t a (Maybe b)
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs

toList :: Trie t => t a (Maybe b) -> [([a], b)]
toList t = case valueIn t of
    Just y -> ([], y) : lower
    Nothing -> lower
  where
    lower = concatMap goDown $ anyChild t
    goDown (x, t') = map (addChar x) $ toList t'
    addChar x (xs, y) = (x:xs, y)

fromLang :: (Eq a, TrieM t) => [[a]] -> t a (Maybe ())
fromLang xs = fromList [(x, ()) | x <- xs]
