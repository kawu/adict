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
, toList
) where

import Prelude hiding (lookup)
import Control.Monad ((>=>))
import Control.Applicative ((<$>))
import Data.List (groupBy, sortBy, find, foldl')
import Data.Function (on)
import qualified Data.Vector as V
import Data.Binary
import Data.Vector.Binary
import Control.Parallel.Strategies

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

-- | Substitute child for a given character. 
subChild :: Eq a => Trie a b -> a -> Trie a b -> Trie a b
subChild (Trie y cs) x newChild =
    cs'' `seq` cs''' `seq` Trie y cs'''
  where
    cs' = filter ((x/=).fst) (V.toList cs)
    cs'' = ((x, newChild):cs') `using` evalList (evalTuple2 rseq rseq)
    cs''' = V.fromList cs''

follow :: Eq a => [a] -> Trie a b -> Maybe (Trie a b)
follow xs t = foldr (>=>) return (map child xs) t

lookup :: Eq a => [a] -> Trie a b -> Maybe b
lookup xs t = follow xs t >>= valueIn

insert :: Eq a => [a] -> b -> Trie a b -> Trie a b
insert [] y (Trie _ cs) = Trie (Just y) cs
insert (x:xs) y trie = subChild trie x . insert xs y $
    case child x trie of
        Just trie' -> trie'
        Nothing    -> empty

fromLang :: Eq a => [[a]] -> Trie a ()
fromLang xs = fromList [(x, ()) | x <- xs]

fromList :: Eq a => [([a], b)] -> Trie a b
fromList xs =
    let update trie (x, y) = insert x y trie
    in  foldl' update empty xs

toList :: Trie a b -> [([a], b)]
toList trie =
    case valueIn trie of
        Just y -> ([], y) : lower
        Nothing -> lower
  where
    lower = concatMap goDown $ anyChild trie
    goDown (x, trie') = map (addChar x) $ toList trie'
    addChar x (xs, y) = (x:xs, y)

-- fromList :: (Eq a, Ord w, ListLike w a) => [(w, b)] -> Trie a b
-- fromList xs =
--     fromSorted $ map (onFst toList) $ nub xs
--   where
--     nub = map head . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
--     onFst f (x, y) = (f x, y)
-- 
-- fromSorted :: Eq a => [([a], b)] -> Trie a b
-- fromSorted (([],v):xs) =
--     let (Trie _ cs) = fromSorted xs
--     in  Trie (Just v) cs
-- fromSorted xs
--     = Trie Nothing $ V.fromList $ map mkTrie
--     $ groupBy ((==) `on` leadingSym) xs
--   where
--     mkTrie :: Eq a => [([a], b)] -> (a, Trie a b)
--     mkTrie xs = ( leadingSym $ head xs
--                 , fromSorted $ map trimLeading xs )
--     leadingSym  ((x:xs), _) = x
--     trimLeading ((x:xs), v) = (xs, v)
