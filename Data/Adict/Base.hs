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

, Adict
, runAdict
, evalAdict
, execAdict
, cost
, thres
, setThres
, modThres
, word
, wordSize
, tell

, Thres
, ThresMod
, thresConst
, thresMargin
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
    { path :: String
    , info :: a }
    deriving Show

instance Eq (Entry a) where
    Entry x _ == Entry y _ = x == y

instance Ord (Entry a) where
    Entry x _ <= Entry y _ = x <= y

instance Functor Entry where
    fmap f (Entry path info) = Entry path (f info)

-- | Monad for approximate dictionary searching:
-- * SearchInfo environment,
-- * String log,
-- * State consist of a current threshold.
type Adict a = RWS SearchInfo String Thres a

data SearchInfo = SearchInfo
    { searchCost     :: Cost Char
    , searchWord     :: Word
    , searchWordSize :: Int }

runAdict :: Cost Char -> Thres -> Word -> Adict a -> (a, String)
runAdict cost th x adict =
    let ~(r, _, w) = runRWS adict (SearchInfo cost x (wordLength x)) th
    in  (r, w)

evalAdict :: Cost Char -> Thres -> Word -> Adict a -> a
evalAdict cost th x = fst . runAdict cost th x

execAdict :: Cost Char -> Thres -> Word -> Adict a -> String
execAdict cost th x = snd . runAdict cost th x

cost :: Adict (Cost Char)
cost = searchCost <$> ask

word :: Adict Word
word = searchWord <$> ask

wordSize :: Adict Int
wordSize = searchWordSize <$> ask

thres :: Adict Thres
thres = get

-- | TODO: Perhaps it should be made strict. Check, if it would
-- make computation faster.
setThres :: Thres -> Adict ()
setThres = put

modThres :: (Thres -> Thres) -> Adict ()
modThres = modify

-- | Changing threshold, when a match was found. It can be assumed,
-- that distance to a matched word (second argument) is not greater
-- than threshold (first argument).
type ThresMod = Thres -> Thres -> Thres

-- | Type synonym for threshold.
type Thres = Double

-- | Do not change threshold.
thresConst :: ThresMod
thresConst th x = th

-- | When a match is found, lower the threshold so that only
-- some margin above the match distance will be searched
-- afterwards.
thresMargin :: Double -> Double -> ThresMod
thresMargin base k th x = min th ((base+x)*(1+k))
