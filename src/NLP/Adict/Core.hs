{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module NLP.Adict.Core
( Word
, Pos
, Weight
, costDefault
, Cost (..)
, (#)
) where

import qualified Data.Vector as V

(#) :: V.Vector a -> Int -> a
x#i = x V.! (i-1)
{-# INLINE (#) #-}

-- | A word parametrized with character type 'a'.
type Word a = V.Vector a

-- | Position in a sentence.
type Pos = Int

-- | Cost of edit operation.  It has to be a non-negative value!
type Weight = Double

-- | Cost represents a cost (or weight) of a symbol insertion, deletion or
-- substitution.  It can depend on edit operation position and on symbol
-- values.
data Cost a = Cost
    { insert :: Pos -> a -> Weight
    , delete :: Pos -> a -> Weight
    , subst  :: Pos -> a -> a -> Weight }

-- | Simple cost function: all edit operations cost 1 unit.
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
