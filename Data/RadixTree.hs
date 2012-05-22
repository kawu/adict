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

instance (Ord a, Binary a, Binary b) => Binary (Trie a b) where
--     put (Trie x ts) = put (x, ts)
--     get = uncurry Trie <$> get
    put t = put (size t) >> mapM_ put (toAscList t)
    get   = fromDistinctAscList <$> get

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

toList :: Trie a b -> [([a], b)]
toList = toListWith id

toAscList :: Ord a => Trie a b -> [([a], b)]
toAscList = toListWith $ sortBy (compare `on` fst)

-- | Generic toList function with additional "operation on children" function.
toListWith :: ([(a, Trie a b)] -> [(a, Trie a b)]) -> Trie a b -> [([a], b)]
toListWith op trie =
    case valueIn trie of
        Just y -> ([], y) : lower
        Nothing -> lower
  where
    lower = concatMap goDown $ op $ anyChild trie
    goDown (x, trie') = map (addChar x) $ toList trie'
    addChar x (xs, y) = (x:xs, y)

fromList :: Eq a => [([a], b)] -> Trie a b
fromList xs =
    let update trie (x, y) = insert x y trie
    in  foldl' update empty xs

fromDistinctAscList :: Eq a => [([a], b)] -> Trie a b
fromDistinctAscList (([],v):xs) =
    let (Trie _ cs) = fromDistinctAscList xs
    in  Trie (Just v) cs
fromDistinctAscList xs
    = Trie Nothing $ V.fromList $ map mkTrie
    $ groupBy ((==) `on` leadingSym) xs
  where
    mkTrie :: Eq a => [([a], b)] -> (a, Trie a b)
    mkTrie xs = ( leadingSym $ head xs
                , fromDistinctAscList $ map trimLeading xs )
    leadingSym  ((x:xs), _) = x
    trimLeading ((x:xs), v) = (xs, v)
