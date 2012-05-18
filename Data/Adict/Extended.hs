module Data.Adict.Extended
( search
, Rule
, Entry (..)
) where

import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Monoid

import Data.RadixTree

-- | Substitution ([a] -> [a]) with given cost.
data Rule a = Rule
    { from  :: [a]
    , cost  :: Double
    , to    :: [a]
    , onBeg :: Bool
    , onEnd :: Bool }

-- | Dinctionary entry.
data Entry a b = Entry
    { word :: [a]
    , info :: b }
    deriving Show

instance Eq a => Eq (Entry a b) where
    Entry x _ == Entry y _ = x == y

instance Ord a => Ord (Entry a b) where
    Entry x _ <= Entry y _ = x <= y

instance Monoid (Rule a) where
    mempty = Rule [] 0.0 [] False False
    mappend (Rule x c y b e) (Rule x' c' y' b' e') =
        Rule x'' c'' y'' b'' e''
      where
        x'' = x ++ x'
        c'' = c + c'
        y'' = y ++ y'
        b'' = case b' of
            True  -> error "right rule onBeg"
            False -> b
        e'' = case e of
            True  -> error "left rule onEnd"
            False -> e'


search :: Ord a => [Rule a] -> Double -> [a]
       -> Trie a b -> [(Entry a b, Double)]
search rs th p trie
     = M.toList $ M.fromListWith min
     $ doSearch ([], 0.0, True) rs th p trie

-- | Find all words in a trie with overall cost lower or equall
-- to given threshold.
doSearch :: Eq a => ([a], Double, Bool) -> [Rule a] -> Double
         -> [a] -> Trie a b -> [(Entry a b, Double)]
doSearch (path, k, _) _ _ [] trie =
    case valueIn trie of
        Just x  -> [(Entry path x, k)]
        Nothing -> []
doSearch (path, k, isBeg) rs th p trie = concat
    [ case follow (to r) trie of
        Just trie'  -> doSearch (path ++ to r, k + cost r, False) rs th
                                (drop (length $ from r) p) trie' 
        Nothing     -> []
    | r <- filter (mathing isBeg p) rs, k + cost r <= th ]

mathing :: Eq a => Bool -> [a] -> Rule a -> Bool
mathing isBeg y r
    | onBeg r && not isBeg = False
    | onEnd r   = from r == y
    | otherwise = from r `isPrefixOf` y 
