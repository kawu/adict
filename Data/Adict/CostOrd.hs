module Data.Adict.CostOrd
( Group (..)
, CostOrd (..)
, Weight
, groupWeight
, costEx
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

costEx :: Int -> CostOrd
costEx n =

    CostOrd insert delete subst posMod

  where

    insert = [Filter (const True) 1]

    delete x
        | C.isPunctuation x = 0.5
        | otherwise         = 1

    subst x =
        [ Filter eq 0
        , Filter ot 1 ]
      where
        eq = (x==)
        ot = not.eq

    posMod k
        | k <= n_2  = 1
        | otherwise = (n - k + 1) ./. (n - n_2 + 1)
    x ./. y = fromIntegral x / fromIntegral y
    n_2 = (n + 1) `div` 2
