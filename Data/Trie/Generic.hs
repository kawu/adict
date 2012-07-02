module Data.Trie.Generic
( Trie (..)
, size
, follow
, lookup
, fromLang
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Data.List (find, foldl')

-- | Generic Trie interface.  Minimal complete definition:
-- empty, anyChild, valueIn, setValue and subChild.
class Trie t where
    empty       :: t a
    anyChild    :: t a -> [(Char, t a)]
    child       :: Char -> t a -> Maybe (t a)

    valueIn     :: t a -> Maybe a
    setValue    :: Maybe a -> t a -> t a

    subChild    :: Char -> t a -> t a -> t a
    insert      :: String -> a -> t a -> t a

    fromList    :: [(String, a)] -> t a
    toList      :: t a -> [(String, a)]

    -- | Default implementations.
    child x t = snd <$> find ((x==).fst) (anyChild t)

    insert [] v trie = setValue (Just v) trie
    insert (x:xs) v trie = subChild x trie . insert xs v $
        case child x trie of
            Just trie' -> trie'
            Nothing    -> empty

    fromList xs =
        let update t (x, v) = insert x v t
        in  foldl' update empty xs

    toList t =
        case valueIn t of
            Just y -> ([], y) : lower
            Nothing -> lower
      where
        lower = concatMap goDown $ anyChild t
        goDown (x, t') = map (addChar x) $ toList t'
        addChar x (xs, y) = (x:xs, y)

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

fromLang :: Trie t => [String] -> t ()
fromLang xs = fromList [(x, ()) | x <- xs]
