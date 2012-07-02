{-# LANGUAGE RecordWildCards #-}

module Data.Trie.MapTrie
( Trie (..)
, size
, empty
, anyChild
, child
, lookup
, follow
, fromList
, fromLang
, toList
) where

import Prelude hiding (lookup)
import Control.Monad ((>=>))
import qualified Data.Map as M
import Data.List (sortBy, foldl')
import Data.Function (on)

data Trie a = Trie
    { valueIn :: Maybe a
    , edgeMap :: M.Map Char (Trie a) }

instance Show a => Show (Trie a) where
    show = show . toList

empty :: Trie a
empty = Trie Nothing (M.fromList [])

size :: Trie a -> Int
size (Trie mv cs)
    | Just _ <- mv  = n + 1
    | otherwise     = n
  where
    n = sum $ map size $ M.elems cs

anyChild :: Trie a -> [(Char, Trie a)]
anyChild Trie{..} = M.assocs edgeMap

child :: Char -> Trie a -> Maybe (Trie a)
child x Trie{..} = x `M.lookup` edgeMap

follow :: String -> Trie a -> Maybe (Trie a)
follow xs t = foldr (>=>) return (map child xs) t

lookup :: String -> Trie a -> Maybe a
lookup xs t = follow xs t >>= valueIn

-- | Substitute child for a given character. 
subChild :: Char -> Trie a -> Trie a -> Trie a
subChild x trie newChild =
    let how _ = Just newChild
    in trie { edgeMap = M.alter how x (edgeMap trie) }

insert :: String -> a -> Trie a -> Trie a
insert [] v trie = trie { valueIn = Just v }
insert (x:xs) v trie = subChild x trie . insert xs v $
    case child x trie of
        Just trie' -> trie'
        Nothing    -> empty

fromLang :: [String] -> Trie ()
fromLang xs = fromList [(x, ()) | x <- xs]

-- | Generic toList function with additional "operation on children" function.
toList :: Trie a -> [(String, a)]
toList trie =
    case valueIn trie of
        Just y -> ([], y) : lower
        Nothing -> lower
  where
    lower = concatMap goDown $ anyChild trie
    goDown (x, trie') = map (addChar x) $ toList trie'
    addChar x (xs, y) = (x:xs, y)

fromList :: [(String, a)] -> Trie a
fromList xs =
    let update trie (x, y) = insert x y trie
    in  foldl' update empty xs
