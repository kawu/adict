module Data.Adict.ShortestPath
( search
) where

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.Maybe (isJust, maybeToList)
import Data.List (group, sortBy)
import Data.Ord (comparing)
import Data.Function (on)

import Data.Adict.Base
import Data.DAWG.Class
import Data.Graph.ShortestPath
import Data.Adict.CostOrd

type NodeID  = Int
data Node = N {-# UNPACK #-} !NodeID {-# UNPACK #-} !Pos
    deriving (Show, Eq, Ord)

{-# INLINE nodeId #-}
nodeId :: Node -> NodeID
nodeId (N x _) = x

type Dag d a = d (Maybe a)

data Which
    = Del Weight
    | Ins Group
    | Sub Group

weight :: Which -> Weight
weight (Del w) = w
weight (Ins g) = groupWeight g
weight (Sub g) = groupWeight g

-- | We can check, if CostOrd satisfies basic properties!
search :: DAWG d => CostOrd -> Double -> Word -> Dag d a
       -> Maybe (String, a, Double)
search cost z x dag = do
    (xs, w) <- minPath z edgesFrom isEnd (N (root dag) 0)
    (form, r) <- entry dag . nub . reverse . map nodeId $ xs
    return (form, r, w)
  where
    nub = map head . group
    edgesFrom (N n i) =
        concatMap follow $ sortBy (comparing weight) $ groups
      where
        follow (Ins (Filter f w)) =
            [ (N m i, w * posMod cost i)
            | (c, m) <- edges dag n, f c ]
        follow (Del w) =
            [ (N n j, w * posMod cost j) ]
        follow (Sub (Filter f w)) =
            [ (N m j, w * posMod cost j)
            | (c, m) <- edges dag n, f c ]
        groups = insGroups ++ delGroups ++ subGroups
        insGroups = Ins <$> insertOrd cost
        delGroups = Del <$> do
            guard (j <= wordLength x)
            return $ deleteOrd cost (x#j)
        subGroups = Sub <$> do
            guard (j <= wordLength x)
            substOrd cost (x#j)
        j = i+1
    isEnd (N n k) = k == wordLength x
                 && isJust (valueIn dag n)
