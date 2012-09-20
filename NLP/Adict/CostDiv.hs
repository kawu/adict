{-# LANGUAGE RecordWildCards #-}

module NLP.Adict.CostDiv
( Group (..)
, CostDiv (..)
, mapWeight
, costDefault

, Sub
, mkSub
, unSub
, SubMap
, subOn
, mkSubMap

, toCost
, toCostInf
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
type Sub a = M.Map Weight (S.Set a)

mkSub :: Ord a => [(a, Weight)] -> Sub a
mkSub xs = M.fromListWith S.union [(w, S.singleton x) | (x, w) <- xs]

unSub :: Ord a => Sub a -> [Group a]
unSub sub =
    [ Filter (`S.member` charSet) weight
    | (weight, charSet) <- M.toAscList sub ]

-- | Susbtitution map for an alphabet.
type SubMap a = M.Map a (Sub a)

subOn :: Ord a => a -> SubMap a -> Sub a
subOn x sm = case M.lookup x sm of
    Just sd -> sd
    Nothing -> M.empty

mkSubMap :: Ord a => [(a, a, Weight)] -> SubMap a
mkSubMap xs = fmap mkSub $
    M.fromListWith (++)
        [ (x, [(y, w)])
        | (x, y, w) <- xs ]

-- | Transform CostDiv to plain Cost function with default weight value.
toCost :: Double -> CostDiv a -> Cost a
toCost defa CostDiv{..} =
    Cost ins del sub
  where
    del k x   = delete x                                * posMod k
    ins k x   = mini [w | Filter f w <- insert,  f x]   * posMod k
    sub k x y = mini [w | Filter f w <- subst x, f y]   * posMod k
    mini []   = defa
    mini xs   = minimum xs

-- | Transform CostDiv to plain Cost function with default weight value
-- set to +Infinity.
toCostInf :: CostDiv a -> Cost a
toCostInf =
    let inf = 1 / 0
    in  toCost inf
