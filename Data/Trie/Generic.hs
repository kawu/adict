module Data.Trie.Generic
( Trie (..)
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

-- | Generic Trie interface.  Minimal complete definition: mkTrie and unTrie.
class Trie t where
    mkTrie      :: Maybe a -> [(Char, t a)] -> t a
    unTrie      :: t a -> (Maybe a, [(Char, t a)])

    empty       :: t a

    setValue    :: Maybe a -> t a -> t a
    valueIn     :: t a -> Maybe a

    anyChild    :: t a -> [(Char, t a)]
    child       :: Char -> t a -> Maybe (t a)

    subChild    :: Char -> t a -> t a -> t a
    insert      :: String -> a -> t a -> t a

    fromTrie    :: Trie s => s a -> t a

    -- | Default implementations.
    empty       = mkTrie Nothing []
    setValue v  = mkTrie v . anyChild 
    valueIn     = fst . unTrie
    anyChild    = snd . unTrie
    child x t   = snd <$> find ((x==).fst) (anyChild t)

    subChild x t c =
        let cs = filter ((x/=).fst) (anyChild t)
        in  mkTrie (valueIn t) ((x, c):cs)

    insert [] v t = setValue (Just v) t
    insert (x:xs) v t = subChild x t . insert xs v $
        case child x t of
            Just t' -> t'
            Nothing -> empty

    fromTrie t = mkTrie (valueIn t)
        [ (x, fromTrie s)
        | (x, s) <- anyChild t ]

size :: Trie t => t a -> Int
size t
    | Just _ <- valueIn t   = n + 1
    | otherwise             = n
  where
    n = sum $ map (size.snd) $ anyChild t

follow :: Trie t => String -> t a -> Maybe (t a)
follow xs t = foldr (>=>) return (map child xs) t

lookup :: Trie t => String -> t a -> Maybe a
lookup xs t = follow xs t >>= valueIn

fromList :: Trie t => [(String, a)] -> t a
fromList xs =
    let update t (x, v) = insert x v t
    in  foldl' update empty xs

toList :: Trie t => t a -> [(String, a)]
toList t = case valueIn t of
    Just y -> ([], y) : lower
    Nothing -> lower
  where
    lower = concatMap goDown $ anyChild t
    goDown (x, t') = map (addChar x) $ toList t'
    addChar x (xs, y) = (x:xs, y)

fromLang :: Trie t => [String] -> t ()
fromLang xs = fromList [(x, ()) | x <- xs]
