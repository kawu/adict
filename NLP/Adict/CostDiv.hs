{-# LANGUAGE RecordWildCards #-}

module NLP.Adict.CostDiv
( Group (..)
, CostDiv (..)
, toCost
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

import NLP.Adict.Core (Pos, Cost(..), Weight)

-- | TODO: Add Choice data contructor together with appropriate
-- implementation: Choice Char Weight
data Group a = Filter
    { predic :: a -> Bool
    , weight :: Weight }

mapWeight :: (Weight -> Weight) -> Group a -> Group a
mapWeight f g = g { weight = f (weight g) }
           
-- | Cost function with edit operations divided with respect to weight.
-- Two operations with the same cost should be assigned to the same group.
data CostDiv a = CostDiv
    { insert ::        [Group a]
    , delete :: a   -> Weight
    , subst  :: a   -> [Group a]
    , posMod :: Pos -> Weight }

costDefault :: Eq a => CostDiv a
costDefault =
    CostDiv insert delete subst posMod
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
{-# INLINABLE costDefault #-}
{-# SPECIALIZE costDefault :: CostDiv Char #-}

-- | Substition desription for some character x.
data SubDsc a = SubDsc
    -- | List of character sets together with a cost of x->y substitution
    -- for any character y in the set.
    { sdFs  :: [(S.Set a, Weight)]
    -- | Sum of sgFs sets.
    , sdTo  :: S.Set a }
    deriving (Show, Read)

mkSD :: Ord a => [(a, Weight)] -> SubDsc a
mkSD xs = SubDsc
    { sdFs = map swap . M.toAscList . fmap S.fromList
           $ M.fromListWith (++) [(w, [x]) | (x, w) <- xs]
    , sdTo = S.fromList (map fst xs) }
  where
    swap (x, y) = (y, x)

sdGroups :: Ord a => SubDsc a -> [Group a]
sdGroups dsc =
    [ Filter (`S.member` charSet) weight
    | (charSet, weight) <- sdFs dsc ]

sdMember :: Ord a => a -> SubDsc a -> Bool
sdMember x dsc
    | x `S.member` sdTo dsc = True
    | otherwise = False

type SubDscMap a = M.Map a (SubDsc a)

subDscOn :: Ord a => a -> SubDscMap a -> SubDsc a
subDscOn x sdm = case M.lookup x sdm of
    Just sd -> sd
    Nothing -> SubDsc [] S.empty

mkSDM :: Ord a => [(a, a, Weight)] -> SubDscMap a
mkSDM xs = fmap mkSD $
    M.fromListWith (++)
        [ (x, [(y, w)])
        | (x, y, w) <- xs ]

-- | Transform CostDiv to plain Cost function.
toCost :: CostDiv a -> Cost a
toCost CostDiv{..} =
    Cost ins del sub
  where
    del k x   = delete x                                * posMod k
    ins k x   = mini [w | Filter f w <- insert,  f x]   * posMod k
    sub k x y = mini [w | Filter f w <- subst x, f y]   * posMod k
    mini []   = 0
    mini xs   = minimum xs
