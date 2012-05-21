module Data.Adict
( levenDist
, levenSearch
, costDefault
, Entry (..)
, Cost (..)
) where

import Data.Ix (range)
import Data.ListLike (ListLike)
import qualified Data.ListLike as L
import qualified Data.Array as A

import Data.RadixTree

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

-- | Levenshtein distance between two strings with given cost function.
levenDist :: (Eq a, ListLike w a) => Cost a -> w -> w -> Double
levenDist cost xs ys = distMem m n where
    distArr = A.array bounds [(x, uncurry dist x) | x <- range bounds]
    bounds  = ((0, 0), (m, n))

    distMem :: Int -> Int -> Double
    distMem i j
        | i >= 0 && j >= 0 = distArr A.! (i, j)
        | i < 0  && j < 0  = 0
        | otherwise        = 1 

    dist i j = minimum
        [ distMem (i-1) (j-1) + (subst cost)  (i, xs ! i) (j, ys ! j)
        , distMem (i-1) j     + (insert cost) (i, xs ! i)
        , distMem i (j-1)     + (delete cost) (j, ys ! j) ]

    m = L.length xs - 1
    n = L.length ys - 1

-- | Find all words in a trie with Levenshtein distance lower or equall to k.
levenSearch :: (Eq a, ListLike w a) => Cost a -> Double -> w
            -> Trie a b -> [(Entry a b, Double)]
levenSearch cost k p trie =
    foundHere ++ foundLower
  where
    foundHere
        | fromIntegral (L.length p) <= k = case valueIn trie of
            Just x  -> [(Entry [] x, fromIntegral $ L.length p)]
            Nothing -> []
        | otherwise = []
    foundLower = concatMap searchLower $ anyChild trie
    searchLower = doSearch cost k 0 distInit p
    distInit = fromIntegral . (+1)

-- | FIXME: Empty pattern case.
doSearch :: (Eq a, ListLike w a) => Cost a -> Double -> Int -> (Int -> Double)
         -> w -> (a, Trie a b) -> [(Entry a b, Double)]
doSearch cost k j distPar p (c, trie) = 
    foundHere ++ map (appendChar c) foundLower
  where
    distArr = A.array bounds [(i, dist i) | i <- range bounds]
    bounds  = (0, m)

    distMem (-1) = fromIntegral $ j + 1
    distMem i    = distArr A.! i

    dist i = minimum
        [ distPar (i-1) + (subst cost)  (i, p ! i) (j, c)
        , distMem (i-1) + (insert cost) (i, p ! i)
        , distPar i     + (delete cost) (j, c) ]

    foundHere
        | distMem m <= k = case valueIn trie of
            Just x  -> [(Entry [c] x, distMem m)]
            Nothing -> []
        | otherwise = []
    foundLower
        | minimum (A.elems distArr) > k = []
        | otherwise = concatMap searchLower $ anyChild trie
      where
        searchLower = doSearch cost k (j+1) distMem p
    -- appendChar c (cs, x) = (c:cs, x)
    appendChar c (Entry cs x, v) = (Entry (c:cs) x, v)

    m = L.length p - 1
