module NLP.Adict.Nearest
( findNearest
) where

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.Maybe (isJust, catMaybes, maybeToList)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.Vector as V
import           Data.Vector.Unboxed (Unbox)

import           NLP.Adict.Core (Pos, Weight, Word, (#))
import           NLP.Adict.CostDiv
import           NLP.Adict.Graph
import qualified Data.DAWG.Static as D
import           Data.DAWG.Static (DAWG, ID)

data Node a = Node
    { nodeID   :: {-# UNPACK #-} !ID
    , nodePos  :: {-# UNPACK #-} !Pos
    , nodeChar :: !(Maybe a) }
    deriving (Show)

proxy :: Node a -> (ID, Pos)
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

mapWeight :: (Weight -> Weight) -> Group a -> Group a
mapWeight f g = g { weight = f (weight g) }

-- | We could check, if CostDiv satisfies basic properties.  On the other
-- hand, we do not do this for plain Cost function.
findNearest
    :: (Enum a, Unbox w)
    => CostDiv a            -- ^ Cost function
    -> Double               -- ^ Threshold
    -> Word a               -- ^ Query word
    -> DAWG a w b
    -> Maybe ([a], b, Double)
findNearest cost z x dag = do
    (xs, w) <- minPath z edgesFrom isEnd (Node (D.rootID dag) 0 Nothing)
    let form = catMaybes . map nodeChar $ xs
    -- TODO: is the assumption, that (length xs > 0), satisfied?
    r <- valueBy dag $ nodeID $ last xs
    return (form, r, w)
  where
    edgesFrom (Node ni i _) =
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
            [ (w, Node (D.rootID m) i (Just c))
            | dawg'  <- maybeToList (D.byID ni dag)
            , (c, m) <- D.edges dawg', f c ]

        follow (Del w) = [(w, Node ni j Nothing)]

        follow (Sub (Filter f w)) =
            [ (w, Node (D.rootID m) j (Just c))
            | dawg'  <- maybeToList (D.byID ni dag)
            , (c, m) <- D.edges dawg', f c ]

    isEnd (Node ni k _) = k == V.length x && isJust (valueBy dag ni)

-- | Get value of a node at the given ID.
valueBy :: (Enum a, Unbox w) => DAWG a w b -> ID -> Maybe b
valueBy dawg i = do
    dawg' <- D.byID i dawg
    D.lookup [] dawg'
