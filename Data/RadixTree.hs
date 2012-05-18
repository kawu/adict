module Data.RadixTree
( Trie ()
, valueIn
, anyChild
, child
, lookup
, follow
, size
, fromList
, fromLang
) where

import Prelude hiding (lookup)
import Control.Monad ((>=>))
import Control.Applicative ((<$>))
import Data.List (groupBy, sortBy, find)
import Data.ListLike (ListLike, toList)
import Data.Function (on)
import qualified Data.Vector as V
import Data.Binary
import Data.Vector.Binary

data Trie a b = Trie (Maybe b) (V.Vector (a, Trie a b)) deriving Show

instance (Binary a, Binary b) => Binary (Trie a b) where
    put (Trie x ts) = put (x, ts)
    get = uncurry Trie <$> get

empty :: Trie a b
empty = Trie Nothing V.empty

size :: Trie a b -> Int
size (Trie mv cs) =
    case mv of
        Just _  -> n + 1
        Nothing -> n
  where
    n = sum $ map (size . snd) $ V.toList cs

valueIn :: Trie a b -> Maybe b
valueIn (Trie mv _) = mv

anyChild :: Trie a b -> [(a, Trie a b)]
anyChild (Trie _ cs) = V.toList cs

child :: Eq a => a -> Trie a b -> Maybe (Trie a b)
child x (Trie _ cs) = snd <$> find ((x==).fst) (V.toList cs)

follow :: Eq a => [a] -> Trie a b -> Maybe (Trie a b)
follow xs t = foldr (>=>) return (map child xs) t

lookup :: Eq a => [a] -> Trie a b -> Maybe b
lookup xs t = follow xs t >>= valueIn

fromLang :: (Eq a, Ord w, ListLike w a) => [w] -> Trie a ()
fromLang xs = fromList [(x, ()) | x <- xs]

fromList :: (Eq a, Ord w, ListLike w a) => [(w, b)] -> Trie a b
fromList xs =
    fromSorted $ map (onFst toList) $ nub xs
  where
    nub = map head . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
    onFst f (x, y) = (f x, y)

fromSorted :: Eq a => [([a], b)] -> Trie a b
fromSorted (([],v):xs) =
    let (Trie _ cs) = fromSorted xs
    in  Trie (Just v) cs
fromSorted xs
    = Trie Nothing $ V.fromList $ map mkTrie
    $ groupBy ((==) `on` leadingSym) xs
  where
    mkTrie :: Eq a => [([a], b)] -> (a, Trie a b)
    mkTrie xs = ( leadingSym $ head xs
                , fromSorted $ map trimLeading xs )
    leadingSym  ((x:xs), _) = x
    trimLeading ((x:xs), v) = (xs, v)
