module Data.Adict.ShortestPath
( search
) where

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.Maybe (isJust, maybeToList, catMaybes)
import Data.List (group, sortBy)
import Data.Ord (comparing)
import Data.Function (on)

import Data.Adict.Base
import Data.DAWG.Class
import Data.Graph.ShortestPath
import Data.Adict.CostOrd

type NodeID  = Int
data Node = Node
    { nodeID   :: {-# UNPACK #-} !NodeID
    , nodePos  :: {-# UNPACK #-} !Pos
    , nodeChar :: !(Maybe Char) }   -- ^ Can we unpack this?
    deriving (Show)

{-# INLINE proxy #-}
proxy :: Node -> (NodeID, Pos)
proxy (Node id k _) = (id, k)

instance Eq Node where
    (==) = (==) `on` proxy

instance Ord Node where
    compare = compare `on` proxy

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
    (xs, w) <- minPath z edgesFrom isEnd (Node (root dag) 0 Nothing)
    let form = catMaybes . map nodeChar $ xs
    r <- valueIn dag $ nodeID $ last xs
    return (form, r, w)
  where
    edgesFrom (Node n i _) =
        concatMap follow $ sortBy (comparing weight) $ groups
      where
        j = i+1

        groups = insGroups ++ delGroups ++ subGroups
        insGroups = Ins <$> insertOrd cost
        delGroups = Del <$> do
            guard (j <= wordLength x)
            return $ deleteOrd cost (x#j)
        subGroups = Sub <$> do
            guard (j <= wordLength x)
            substOrd cost (x#j)

        follow (Ins (Filter f w)) =
            [ ( Node m i (Just c)
              , w * posMod cost i )
            | (c, m) <- edges dag n
            , f c ]

        follow (Del w) =
            [ ( Node n j Nothing
              , w * posMod cost j) ]

        follow (Sub (Filter f w)) =
            [ ( Node m j (Just c)
              , w * posMod cost j )
            | (c, m) <- edges dag n
            , f c ]

    isEnd (Node n k _) = k == wordLength x
                      && isJust (valueIn dag n)
