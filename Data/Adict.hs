module Data.Adict
( Adict
, levenDist
, levenSearch
, simpleSearch
, costDefault
, Entry (..)
, Cost (..)
, Pos
, module Data.RadixTree
) where

import Data.Maybe (catMaybes)
import Data.Ix (range)
import Data.ListLike (ListLike)
import qualified Data.ListLike as L
import qualified Data.Array as A

import Data.RadixTree

type Adict a b = Trie a b

(!) :: ListLike full item => full -> Int -> item
(!) = L.index

-- | Position.
type Pos = Int

-- | Cost a represents a cost (or weight) of @a@ symbol insertion, deletion
-- or substitution.  It can depend on edit operation position and on symbol
-- values.
data Cost a = Cost
    { insert :: (Pos, a) -> Double
    , delete :: (Pos, a) -> Double
    , subst  :: (Pos, a) -> (Pos, a) -> Double }

-- | Simple cost function: all edit operations cost 1.
costDefault :: Eq a => Cost a
costDefault =
    Cost insert delete subst
  where
    insert _ = 1
    delete _ = 1
    subst (_, x) (_, y)
        | x == y    = 0
        | otherwise = 1

-- | Dinctionary entry.
data Entry a b = Entry
    { word :: [a]
    , info :: b }
    deriving Show

instance Eq a => Eq (Entry a b) where
    Entry x _ == Entry y _ = x == y

instance Ord a => Ord (Entry a b) where
    Entry x _ <= Entry y _ = x <= y

instance Functor (Entry a) where
    fmap f (Entry word info) = Entry word (f info)

(#) :: ListLike w a => w -> Int -> a
x#i = x!(i-1)

-- | Restricted generalized edit distance between two strings with
-- given cost function.
levenDist :: (Eq a, ListLike w a) => Cost a -> w -> w -> Double
levenDist cost x y =
    dist' m n
  where
    dist' i j = distA A.! (i, j)
    distA = A.array bounds [(k, uncurry dist k) | k <- range bounds]
    bounds  = ((0, 0), (m, n))
    m = L.length x
    n = L.length y

    dist 0 0 = 0
    dist i 0 = dist' (i-1) 0 + (delete cost) (i, x#i)
    dist 0 j = dist' 0 (j-1) + (insert cost) (j, y#j)
    dist i j = minimum
        [ dist' (i-1) (j-1) + (subst cost)  (i, x#i) (j, y#j)
        , dist' (i-1) j     + (delete cost) (i, x#i)
        , dist' i (j-1)     + (insert cost) (j, y#j) ]

-- | Find all words within a list with restricted generalized edit distance
-- from x lower or equall to k.
simpleSearch :: (Eq a, ListLike w a) => Cost a -> Double -> w
             -> [(w, b)] -> [(Entry a b, Double)]
simpleSearch cost k x set =
    catMaybes $ map check set
  where
    check (y, v)
        | dist <= k = Just (Entry (L.toList y) v, dist)
        | otherwise = Nothing
      where
        dist = levenDist cost x y

-- | Find all words within a trie with restricted generalized edit distance
-- lower or equall to k.
levenSearch :: (Eq a, ListLike w a) => Cost a -> Double -> w
            -> Trie a b -> [(Entry a b, Double)]
levenSearch cost k x trie =
    foundHere ++ foundLower
  where
    foundHere
        | dist' m <= k = case valueIn trie of
            Just x  -> [(Entry [] x, dist' m)]
            Nothing -> []
        | otherwise = []
    foundLower
        | minimum (A.elems distV) > k = []
        | otherwise = concatMap searchLower $ anyChild trie
    searchLower = levenSearch' cost k 1 dist' x

    dist' = (A.!) distV 
    distV = A.array bounds [(i, dist i) | i <- range bounds]
    bounds = (0, m)
    m = L.length x

    dist 0 = 0
    dist i = dist' (i-1) + (delete cost) (i, x#i)

levenSearch' :: (Eq a, ListLike w a)
             => Cost a -> Double -> Int -> (Int -> Double) -> w
             -> (a, Trie a b) -> [(Entry a b, Double)]
levenSearch' cost k j distP x (c, trie) =
    foundHere ++ map (appendChar c) foundLower
  where
    foundHere
        | dist' m <= k = case valueIn trie of
            Just x  -> [(Entry [c] x, dist' m)]
            Nothing -> []
        | otherwise = []
    foundLower
        | minimum (A.elems distV) > k = []
        | otherwise = concatMap searchLower $ anyChild trie
    searchLower = levenSearch' cost k (j+1) dist' x
    appendChar c (Entry cs x, v) = (Entry (c:cs) x, v)

    dist' = (A.!) distV 
    distV = A.array bounds [(i, dist i) | i <- range bounds]
    bounds  = (0, m)
    m = L.length x

    dist 0 = distP 0  + (insert cost) (j, c)
    dist i = minimum
        [ distP (i-1) + (subst cost)  (i, x#i) (j, c)
        , dist' (i-1) + (delete cost) (i, x#i)
        , distP i     + (insert cost) (j, c) ]
