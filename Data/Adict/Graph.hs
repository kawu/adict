{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Adict.Graph
( search
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard)
import Data.Function (on)
import Data.Maybe (maybeToList, fromJust, catMaybes)
import Data.List (minimumBy, foldl')
import qualified Data.Set as S

import Data.Trie.Class hiding (insert)
import Data.Adict.Base

import Debug.Trace (trace)

-- | Edit distance vector for some trie node.
type CostVect = [(Pos, Double)]

-- | Minimum value in the cost vector.
minCost :: CostVect -> (Pos, Double)
minCost = minimumBy (compare `on` snd)

-- | Graph node.
data Node t = Node
    -- | Trie node.
    { trie      :: t
    -- | Path from root to the trie node (inversed).
    , path      :: String
    -- | Cost vector computed for the trie node.
    , costVect  :: CostVect
    -- | Depth of the trie node in the entire trie.
    , depth     :: Pos }

-- | Node representative used for ordering in a search graph. 
proxy :: Node t -> (Double, Pos, String)
proxy = (,,) <$> snd.minCost.costVect <*> negate.depth <*> path

instance Eq (Node t) where
    (==) = (==) `on` proxy

instance Ord (Node t) where
    compare = compare `on` proxy

-- rootNode :: t -> Node t
-- rootNode t = Node t [] [(0, 0)] 0

-- | Type synonym for threshold.
type Thres = Double

type PQueue t a = S.Set (Node (t (Maybe a)))

search :: Trie t => Cost Char -> Thres -> Word
       -> t (Maybe a) -> [(Entry a, Double)]
search cost th x trie =
    here ++ lower
  where
    costVect = initVect cost th x
    -- | TODO: Abstract 'here' and 'hereLower' as a separate function.
    here = maybeToList $ do
        (k, dist) <- maybeLast costVect
        guard (k == wordSize x)
        v <- valueIn trie
        return (Entry [] v, dist)
    queue = S.singleton (Node trie [] costVect 0)
    lower = search' cost th x queue

search' :: Trie t => Cost Char -> Thres -> Word 
        -> PQueue t a -> [(Entry a, Double)]
search' cost th x q
    | S.null q  = []
    | otherwise = here ++ search' cost th x q''
  where
    (n, q') = fromJust $ S.minView q
    q'' = foldl' (flip S.insert) q' (successors cost th x n)
    here = maybeToList $ do
        (k, dist) <- maybeLast (costVect n)
        guard (k == wordSize x)
        v <- valueIn (trie n)
        return (Entry (reverse $ path n) v, dist)

successors :: Trie t => Cost Char -> Thres -> Word
           -> Node (t a) -> [Node (t a)]
successors cost th x Node{..} = catMaybes
    [ case nextVect c costVect of
        [] -> Nothing
        xs -> Just $ Node t (c:path) xs (depth+1)
    | (c, t) <- anyChild trie ]
  where
    nextVect = computeVect cost th x (depth+1)  -- ^ FIXME depth+1?

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs

-- | TODO: Move definitions below from here (and Adict.Fast) to a
-- separate module.
initVect :: Cost Char -> Thres -> Word -> CostVect
initVect cost th x = mapDel cost th x 0 [(0, 0)]

computeVect :: Cost Char -> Thres -> Word -> Pos -> Char
            -> CostVect -> CostVect
computeVect cost th x j c costVect = mapDel cost th x j $ merge
    [x | x@(_, !v) <- map ins costVect, v <= th]
    [x | x@(_, !v) <- map sub costVect, v <= th]
  where
    m = wordSize x
    {-# INLINE ins #-}
    ins (!i, !v) =
        let !v' = v + (insert cost) i (j, c)
        in  (i, v')
    {-# INLINE sub #-}
    sub (!k, !v)
        | k < m  =
            let !i = k + 1
                !v' = v + (subst cost)  (i, x#i) (j, c)
            in  (i, v')
        | otherwise = (k, th + 1.0) -- ^ Is it faster than (i, th + 1.0)?

mapDel :: Cost Char -> Thres -> Word -> Pos -> CostVect -> CostVect
mapDel cost th w j = doIt
  where
    doIt []     = []
    doIt (x:xs) =
        let xs' = merge (x:xs) (del x)
        in  head xs' : doIt (tail xs')
    m = wordSize w
    {-# INLINE del #-}
    del (k, v)
        | i <= m && u <= th = [(i, u)]
        | otherwise = []
      where
        (i, u) = (k + 1, v + (delete cost) (i, w#i) j)

merge :: CostVect -> CostVect -> CostVect
merge xs [] = xs
merge [] ys = ys
merge xs@((i, v):xs') ys@((j, w):ys')
    | i == j =
            let !u = min v w
            in (i, u) : merge xs' ys'
    | i < j  = (i, v) : merge xs' ys
    | i > j  = (j, w) : merge xs  ys'
