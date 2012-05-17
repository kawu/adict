module Data.Adict.Extended
( search
, Rule
, Res
) where

import Data.List (isPrefixOf)
import qualified Data.Map as M

import Data.RadixTree

-- | Substitution ([a] -> [a]) with given cost.
type Rule a = ([a], Double, [a])

-- | Search result.
type Res a b = ([a], b)

search :: (Ord a, Ord b) => [Rule a] -> Double -> [a]
       -> Trie a b -> [(Res a b, Double)]
search rs th p trie
     = M.toList $ M.fromListWith min
     $ doSearch ([], 0.0) rs th p trie

-- | Find all words in a trie with overall cost lower or equall
-- to given threshold.
doSearch :: Eq a => ([a], Double) -> [Rule a] -> Double -> [a]
         -> Trie a b -> [(Res a b, Double)]
doSearch (path, k) _ _ [] trie =
    case valueIn trie of
        Just x  -> [((path, x), k)]
        Nothing -> []
doSearch (path, k) rs th p trie = concat
    [ case follow x' trie of
        Just trie'  -> doSearch (path ++ x', k + k') rs th
                                (drop (length x) p) trie' 
        Nothing     -> []
    | r@(x, k', x') <- findMathing rs p, k + k' <= th ]

findMathing :: Eq a => [Rule a] -> [a] -> [Rule a]
findMathing rs y = [r | r@(x, _, _) <- rs , x `isPrefixOf` y]
