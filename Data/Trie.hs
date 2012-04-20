module Data.Trie
( Trie ()
, valueIn
, anyChild
, child
, lookup
, size
, fromList
, fromLang
) where

import Prelude hiding (lookup)
import Control.Monad ((>=>))
import Control.Applicative ((<$>))
import Data.List (groupBy, sortBy, find)
import Data.Function (on)
import qualified Data.Vector as V
import qualified Data.Text as T

data Trie a = Trie (Maybe a) (V.Vector (Char, Trie a)) deriving Show

empty :: Trie a
empty = Trie Nothing V.empty

size :: Trie a -> Int
size (Trie mv cs) =
    case mv of
        Just _  -> n + 1
        Nothing -> n
  where
    n = sum $ map (size . snd) $ V.toList cs

valueIn :: Trie a -> Maybe a
valueIn (Trie mv _) = mv

anyChild :: Trie a -> [(Char, Trie a)]
anyChild (Trie _ cs) = V.toList cs

child :: Char -> Trie a -> Maybe (Trie a)
child x (Trie _ cs) = snd <$> find ((x==).fst) (V.toList cs)

lookup :: String -> Trie a -> Maybe a
lookup xs t = foldr (>=>) return (map child xs) t >>= valueIn

fromLang :: [T.Text] -> Trie ()
fromLang xs = fromList [(x, ()) | x <- xs]

fromList :: [(T.Text, a)] -> Trie a
fromList xs =
    fromSorted $ map (onFst T.unpack) $ nub xs
  where
    nub = map head . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
    onFst f (x, y) = (f x, y)

fromSorted :: [(String, a)] -> Trie a
fromSorted (("",v):xs) =
    let (Trie _ cs) = fromSorted xs
    in  Trie (Just v) cs
fromSorted xs
    = Trie Nothing $ V.fromList $ map mkTrie
    $ groupBy ((==) `on` leadingChar) xs
  where
    mkTrie :: [(String, a)] -> (Char, Trie a)
    mkTrie xs = ( leadingChar $ head xs
                , fromSorted $ map trimLeading xs )
    leadingChar ((x:xs), _) = x
    trimLeading ((x:xs), v) = (xs, v)
