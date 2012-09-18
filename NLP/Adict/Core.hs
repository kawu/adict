{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module NLP.Adict.Core
( Word
, Pos
, Weight (unWeight)
, costDefault
, Cost (..)
, (#)
) where

import qualified Data.Vector as V

(#) :: V.Vector a -> Int -> a
x#i = x V.! (i-1)
{-# INLINE (#) #-}

type Word a = V.Vector a

-- | Position.
type Pos = Int

-- | Non-negative weight (cost) of edit operation.
newtype Weight = Weight { unWeight :: Double }
    deriving (Show, Read, Eq, Ord)

mkWeight :: Double -> Weight
mkWeight w
    | w < 0     = error "mkWeight: negative weight"
    | otherwise = Weight w

instance Num Weight where
    Weight u * Weight w = Weight (u * w)
    Weight u + Weight w = Weight (u + w)
    Weight u - Weight w = mkWeight (u - w)
    negate _            = error "negate weight" 
    abs                 = id    -- ^ We know its non-negative
    signum (Weight x)
        | x == 0    = 0
        | x >  0    = 1
        | otherwise = error "signum weight"
    fromInteger = mkWeight . fromInteger

instance Fractional Weight where
    Weight u / Weight w = Weight (u / w)
    fromRational = mkWeight . fromRational

-- | Cost represents a cost (or weight) of a symbol insertion, deletion or
-- substitution.  It can depend on edit operation position and on symbol
-- values.
data Cost a = Cost
    { insert :: Pos -> a -> Weight
    , delete :: Pos -> a -> Weight
    , subst  :: Pos -> a -> a -> Weight }

-- | Simple cost function: all edit operations cost 1.
costDefault :: Eq a => Cost a
costDefault =
    Cost _insert _delete _subst
  where
    _insert _ _ = 1
    _delete _ _ = 1
    _subst _ x y
        | x == y    = 0
        | otherwise = 1
{-# INLINABLE costDefault #-}
{-# SPECIALIZE costDefault :: Cost Char #-}
