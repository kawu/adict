{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Adict.Base
( costDefault
, Cost (..)
, Pos
, Entry (..)

, Word
, (#)
, wordLength
, toString
, fromString
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Vector.Unboxed as U
import Control.Monad.Trans (lift)
import Control.Monad.RWS (runRWS, RWS)
import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Writer.Class (tell)

-- | Word type.
type Word = U.Vector Char

{-# INLINE wordLength #-}
wordLength :: Word -> Int
wordLength = U.length

{-# INLINE (#) #-}
(#) :: Word -> Int -> Char
x#i = x U.! (i-1)

toString :: Word -> String
toString = U.toList

fromString :: String -> Word
fromString = U.fromList

-- | Position.
type Pos = Int

-- | Cost represents a cost (or weight) of a symbol insertion, deletion or
-- substitution.  It can depend on edit operation position and on symbol
-- values.
data Cost = Cost
    { insert :: Pos -> Char -> Double
    , delete :: Pos -> Char -> Double
    , subst  :: Pos -> Char -> Char -> Double }

-- | Simple cost function: all edit operations cost 1.
costDefault :: Cost
costDefault =
    Cost insert delete subst
  where
    insert _ _ = 1
    delete _ _ = 1
    subst _ x y
        | x == y    = 0
        | otherwise = 1

-- | Dinctionary entry.
data Entry a = Entry
    { form :: String
    , info :: a }
    deriving Show

instance Eq (Entry a) where
    Entry x _ == Entry y _ = x == y

instance Ord (Entry a) where
    Entry x _ <= Entry y _ = x <= y

instance Functor Entry where
    fmap f (Entry path info) = Entry path (f info)
