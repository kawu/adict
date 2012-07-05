{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Adict.Base
( costDefault
, Cost (..)
, Thres
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
, word
, wordSize
, record
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Vector.Unboxed as U
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Writer (Writer, runWriter, tell)

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

-- | Type synonym for threshold.
type Thres = Double

-- | Rearder monad for storage of information unchanging throughout
-- the search.
type Adict a = ReaderT SearchInfo (Writer String) a

data SearchInfo = SearchInfo
    { searchCost     :: Cost Char
    , searchThres    :: Thres
    , searchWord     :: Word
    , searchWordSize :: Int }

runAdict :: Cost Char -> Thres -> Word -> Adict a -> (a, String)
runAdict cost th x adict
    = runWriter $ runReaderT adict
    $ SearchInfo cost th x (wordLength x)

evalAdict :: Cost Char -> Thres -> Word -> Adict a -> a
evalAdict cost th x = fst . runAdict cost th x

execAdict :: Cost Char -> Thres -> Word -> Adict a -> String
execAdict cost th x = snd . runAdict cost th x

cost :: Adict (Cost Char)
cost = searchCost <$> ask

thres :: Adict Thres
thres = searchThres <$> ask

word :: Adict Word
word = searchWord <$> ask

wordSize :: Adict Int
wordSize = searchWordSize <$> ask

record :: String -> Adict ()
record = lift . tell
