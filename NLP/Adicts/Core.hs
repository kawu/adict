{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module NLP.Adicts.Core
( Word
, Pos
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

-- | Cost represents a cost (or weight) of a symbol insertion, deletion or
-- substitution.  It can depend on edit operation position and on symbol
-- values.
data Cost a = Cost
    { insert :: Pos -> a -> Double
    , delete :: Pos -> a -> Double
    , subst  :: Pos -> a -> a -> Double }

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
