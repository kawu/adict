{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Adict.Base
( costDefault
, Cost (..)
, Pos
, Entry (..)

, Word
, (#)
, wordSize
, toString
, fromString
) where

import qualified Data.Vector.Unboxed as U

-- | Word type.
type Word = U.Vector Char

{-# INLINE wordSize #-}
wordSize :: Word -> Int
wordSize = U.length

{-# INLINE (#) #-}
(#) :: Word -> Int -> Char
x#i = x U.! (i-1)

toString :: Word -> String
toString = U.toList

fromString :: String -> Word
fromString = U.fromList

-- | Position.
type Pos = Int

-- | Cost a represents a cost (or weight) of @a@ symbol insertion, deletion
-- or substitution.  It can depend on edit operation position and on symbol
-- values.
data Cost a = Cost
    { insert ::  Pos     -> (Pos, a) -> Double
    , delete :: (Pos, a) ->  Pos     -> Double
    , subst  :: (Pos, a) -> (Pos, a) -> Double }

-- | Simple cost function: all edit operations cost 1.
costDefault :: Eq a => Cost a
costDefault =
    Cost insert delete subst
  where
    insert _ _ = 1
    delete _ _ = 1
    subst (_, x) (_, y)
        | x == y    = 0
        | otherwise = 1

-- | Dinctionary entry.
data Entry a = Entry
    { word :: String
    , info :: a }
    deriving Show

instance Eq (Entry a) where
    Entry x _ == Entry y _ = x == y

instance Ord (Entry a) where
    Entry x _ <= Entry y _ = x <= y

instance Functor Entry where
    fmap f (Entry word info) = Entry word (f info)
