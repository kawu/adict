module Data.Adict.Extended
( search
, Rule
, Entry (..)
) where

import Data.List (isPrefixOf)
import qualified Data.Map as M

import Data.RadixTree

-- | Substitution ([a] -> [a]) with given cost.
type Rule a = ([a], Double, [a])

-- | Dinctionary entry.
data Entry a b = Entry
    { word :: [a]
    , info :: b }
    deriving Show

instance Eq a => Eq (Entry a b) where
    Entry x _ == Entry y _ = x == y

instance Ord a => Ord (Entry a b) where
    Entry x _ <= Entry y _ = x <= y

search :: Ord a => [Rule a] -> Double -> [a]
       -> Trie a b -> [(Entry a b, Double)]
search rs th p trie
     = M.toList $ M.fromListWith min
     $ doSearch ([], 0.0) rs th p trie

-- | Find all words in a trie with overall cost lower or equall
-- to given threshold.
doSearch :: Eq a => ([a], Double) -> [Rule a] -> Double -> [a]
         -> Trie a b -> [(Entry a b, Double)]
doSearch (path, k) _ _ [] trie =
    case valueIn trie of
        Just x  -> [(Entry path x, k)]
        Nothing -> []
doSearch (path, k) rs th p trie = concat
    [ case follow x' trie of
        Just trie'  -> doSearch (path ++ x', k + k') rs th
                                (drop (length x) p) trie' 
        Nothing     -> []
    | r@(x, k', x') <- findMathing rs p, k + k' <= th ]

findMathing :: Eq a => [Rule a] -> [a] -> [Rule a]
findMathing rs y = [r | r@(x, _, _) <- rs , x `isPrefixOf` y]
