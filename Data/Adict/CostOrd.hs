module Data.Adict.CostOrd
( Group (..)
, CostOrd (..)
, Weight
, groupWeight
, costDefault
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Char as C

import Data.Adict.Base (Pos)

-- | Identifier of DAG node.
type NodeId = Int

-- | Weight of edit operation
type Weight = Double

-- | TODO
data Group = Filter (Char -> Bool) Weight
--            | Choice Char Weight

groupWeight :: Group -> Weight
-- groupWeight (Choice _ w) = w
groupWeight (Filter _ w) = w
           
-- | Each member function should return results in ascending order
-- with respect to weights.
data CostOrd = CostOrd
    { insertOrd :: [Group]
    , deleteOrd :: Char -> Weight
    , substOrd  :: Char -> [Group]
    , posMod    :: Pos  -> Double }

costDefault :: CostOrd
costDefault =
    CostOrd insert delete subst posMod
  where
    insert   = [Filter (const True) 1]
    delete _ = 1
    subst x  =
        [ Filter eq 0
        , Filter ot 1 ]
      where
        eq = (x==)
        ot = not.eq
    posMod = const 1
