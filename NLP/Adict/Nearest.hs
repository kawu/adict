module NLP.Adict.Nearest
( search
) where

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.Maybe (isJust, catMaybes)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.Vector as V

import NLP.Adict.Core (Pos, Weight, Word, (#))
import NLP.Adict.CostDiv
import NLP.Adict.DAWG
import NLP.Adict.Graph

type NodeID  = Int
data Node a = Node
    { nodeID   :: {-# UNPACK #-} !NodeID
    , nodePos  :: {-# UNPACK #-} !Pos
    , nodeChar :: !(Maybe a) }
    deriving (Show)

proxy :: Node a -> (NodeID, Pos)
proxy n = (nodeID n, nodePos n)
{-# INLINE proxy #-}

instance Eq (Node a) where
    (==) = (==) `on` proxy

instance Ord (Node a) where
    compare = compare `on` proxy

data Which a
    = Del Weight
    | Ins (Group a)
    | Sub (Group a)

weightOf :: Which a -> Weight
weightOf (Del w) = w
weightOf (Ins g) = weight g
weightOf (Sub g) = weight g
{-# INLINE weightOf #-}

-- | We can check, if CostDiv satisfies basic properties.  On the other
-- hand, we do not do this for plain Cost function.
search :: Show a => CostDiv a -> Double -> Word a -> DAWGD a b -> Maybe ([a], b, Double)
search cost z x dag = do
    (xs, w) <- minPath z edgesFrom isEnd (Node (root dag) 0 Nothing)
    let form = catMaybes . map nodeChar $ xs
    r <- valueIn dag $ nodeID $ last xs
    return (form, r, w)
  where
    edgesFrom (Node n i _) =
        concatMap follow $ sortBy (comparing weightOf) groups
      where
        j = i+1

        groups = insGroups ++ delGroups ++ subGroups
        insGroups = Ins . mapWeight (*posMod cost i) <$>
            insert cost
        delGroups = Del . (*posMod cost j) <$> do
            guard (j <= V.length x)
            return $ delete cost (x#j)
        subGroups = Sub . mapWeight (*posMod cost j) <$> do
            guard (j <= V.length x)
            subst cost (x#j)

        follow (Ins (Filter f w)) =
            [ (w, Node m i (Just c))
            | (c, m) <- edges dag n
            , f c ]

        follow (Del w) = [(w, Node n j Nothing)]

        follow (Sub (Filter f w)) =
            [ (w, Node m j (Just c))
            | (c, m) <- edges dag n
            , f c ]

    isEnd (Node n k _) = k == V.length x && isJust (valueIn dag n)
