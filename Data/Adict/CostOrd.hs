module Data.Adict.CostOrd
( Group (..)
, CostOrd (..)
, Weight
, groupWeight
, mapWeight
, costDefault

, SubDsc (..)
, mkSD
, sdGroups
, sdMember
, SubDscMap
, subDscOn
, mkSDM
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C
import Control.Applicative ((<$>), (<*>))

import Data.Adict.Base (Pos)

-- | Identifier of DAG node.
type NodeId = Int

-- | Weight of edit operation
type Weight = Double

-- | TODO: Add Choice data contructor together with appropriate
-- implementation.
data Group = Filter (Char -> Bool) Weight
--            | Choice Char Weight

groupWeight :: Group -> Weight
groupWeight (Filter _ w) = w

mapWeight :: (Weight -> Weight) -> Group -> Group
mapWeight f (Filter g w) = Filter g (f w)
           
-- | Each member function (i.e., insertOrd and substOrd) should return
-- results in ascending order with respect to weights.
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

-- -- | Transform CostOrd to plain Cost function.
-- toCost :: CostOrd -> Cost
-- toCost = undefined

-- | Substition desription for some character x.
data SubDsc = SubDsc
    -- | List of character sets together with a cost of x->y substitution
    -- for any character y in the set.
    { sdFs  :: [(S.Set Char, Double)]
    -- | Sum of sgFs sets.
    , sdTo  :: S.Set Char }
    deriving (Show, Read)

mkSD :: [(Char, Double)] -> SubDsc
mkSD xs = SubDsc
    { sdFs = map swap . M.toAscList . fmap S.fromList
           $ M.fromListWith (++) [(w, [x]) | (x, w) <- xs]
    , sdTo = S.fromList (map fst xs) }
  where
    swap (x, y) = (y, x)

sdGroups :: SubDsc -> [Group]
sdGroups dsc =
    [ Filter (`S.member` charSet) weight
    | (charSet, weight) <- sdFs dsc ]

sdMember :: Char -> SubDsc -> Bool
sdMember x dsc
    | x `S.member` sdTo dsc = True
    | otherwise = False

type SubDscMap = M.Map Char SubDsc

subDscOn :: Char -> SubDscMap -> SubDsc
subDscOn x sdm = case M.lookup x sdm of
    Just sd -> sd
    Nothing -> SubDsc [] S.empty

mkSDM :: [(Char, Char, Double)] -> SubDscMap
mkSDM xs = fmap mkSD $
    M.fromListWith (++)
        [ (x, [(y, w)])
        | (x, y, w) <- xs ]
