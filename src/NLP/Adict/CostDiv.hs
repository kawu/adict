{-# LANGUAGE RecordWildCards #-}

-- | Alternative cost representation with individual cost components
-- divided into groups with respect to operation weights.  

module NLP.Adict.CostDiv
(
-- * CostDiv
  Group (..)
, CostDiv (..)
, costDefault

-- * Helper functions for CostDiv construction 
, Sub
, mkSub
, unSub
, SubMap
, subOn
, mkSubMap

-- * Conversion to standard representation
, toCost
, toCostInf
) where

import qualified Data.Set as S
import qualified Data.Map as M

import NLP.Adict.Core (Pos, Cost(..), Weight)


-- TODO: Add Choice data contructor.

-- | A Group describes a weight of some edit operation in which a character
-- satistying the predicate is involved.  This data structure is meant to
-- collect all characters which determine the same operation weight.
data Group a = Filter {
    -- | The predicate determines which characters belong to the group.
    predic :: a -> Bool,
    -- | Weight of the edit operation in which a character satisfying the
    -- predicate is involved.
    weight :: Weight  }

-- | Cost function with edit operations divided with respect to weight.
-- Two operations of the same type and with the same weight should be
-- assigned to the same group.
data CostDiv a = CostDiv {
    -- | Cost of the character insertion divided into groups with
    -- respect to operation weights.
    insert ::        [Group a],
    -- | Cost of the character deletion.
    delete :: a   -> Weight,
    -- | Cost of the character substitution.  For each source character
    -- there can be a different list of groups involved. 
    subst  :: a   -> [Group a],
    -- | Cost of each edit operation is multiplied by the position modifier.
    -- For example, the cost of @\'a\'@ character deletion on position @3@
    -- is computed as @delete \'a\' * posMod 3@.
    posMod :: Pos -> Weight }

-- | Default cost with all edit operations having the unit weight.
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

-- | Substition description for some unspecified source character.
type Sub a = M.Map Weight (S.Set a)

-- | Construct the substitution descrition from the list of (character @y@,
-- substition weight from @x@ to @y@) pairs for some unspecified character
-- @x@.  Characters will be grouped with respect to weight.
mkSub :: Ord a => [(a, Weight)] -> Sub a
mkSub xs = M.fromListWith S.union [(w, S.singleton x) | (x, w) <- xs]

-- | Extract the list of groups (each group with unique weight) from the
-- substitution description.
unSub :: Ord a => Sub a -> [Group a]
unSub sub =
    [ Filter (`S.member` charSet) weight
    | (weight, charSet) <- M.toAscList sub ]

-- | A susbtitution map which covers all substition operations.
type SubMap a = M.Map a (Sub a)

-- | Substitution description for the given character in the substitution map.
-- In other words, the function returns information how the input character can
-- be replaced with other characters from the alphabet.
subOn :: Ord a => a -> SubMap a -> Sub a
subOn x sm = case M.lookup x sm of
    Just sd -> sd
    Nothing -> M.empty

-- | Construct the substitution map from the list of (@x@, @y@, weight of
-- @x -> y@ substitution) tuples.
mkSubMap :: Ord a => [(a, a, Weight)] -> SubMap a
mkSubMap xs = fmap mkSub $
    M.fromListWith (++)
        [ (x, [(y, w)])
        | (x, y, w) <- xs ]

-- | Transform CostDiv to plain Cost function using the default weight value
-- for all operations unspecified in the input cost.
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
-- set to @+Infinity@.
toCostInf :: CostDiv a -> Cost a
toCostInf =
    let inf = 1 / 0
    in  toCost inf
