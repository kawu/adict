{-# LANGUAGE RecordWildCards #-}

-- QuickCheck properties, which should be satisfied by Adict.

module Data.Adict.Check
( CostDesc (..)
, fromDesc
, Lang
, arbitraryLang
, getWords
, propConsistency
) where

import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Test.QuickCheck

import qualified Data.Adict.Brute as B
import qualified Data.Adict.Slow as S
import qualified Data.Adict.Fast as F
import qualified Data.Trie.Trie as T
import qualified Data.Trie.Class as C
import Data.Adict.Base

-- | Check parameters.
posRange  = (0, 4)	-- ^ Position of random edit op
valRange  = (0, 10)	-- ^ Weight of edit op 
descRange = (0, 25)	-- ^ Number of random edit ops of given type
langRange = (0, 25)	-- ^ Size of language

type Val = Double

-- | Helper structure with Arbitrary instance (implementation below),
-- which can be transformed to an Adict Cost function.
data CostDesc = CostDesc
    { insD :: M.Map (Pos, Char) Val
    , delD :: M.Map (Pos, Char) Val
    , subD :: M.Map (Pos, Char, Char) Val }
    deriving Show

-- | Construct Cost function from a description structure.
fromDesc :: CostDesc -> Cost
fromDesc CostDesc{..} = Cost ins del sub
  where
    ins i x = fromJust $ (i, x) `M.lookup` insD <|> return 1
    del i x = fromJust $ (i, x) `M.lookup` delD <|> return 1
    sub i x y = fromJust $ (i, x, y) `M.lookup` subD  <|> return (sub' x y)
    sub' x y
        | x == y    = 0
        | otherwise = 1

arbitraryPos :: Gen Pos
arbitraryPos = choose posRange

arbitraryVal :: Gen Val
arbitraryVal = choose valRange

instance Arbitrary CostDesc where
    arbitrary = do
        ins <- M.fromList <$> listOf insElem 
        del <- M.fromList <$> listOf delElem
        sub <- M.fromList <$> listOf subElem
        return $ CostDesc ins del sub
      where 
	insElem  = (,)  <$> arbitraryPos <*> arbitrary
	delElem  = (,)  <$> arbitraryPos <*> arbitrary
	subElem  = (,,) <$> arbitraryPos <*> arbitrary <*> arbitrary
        listOf m = do
            k  <- choose descRange
            vectorOf k ((,) <$> m <*> arbitraryVal)

arbitraryLang :: (Int, Int) -> Gen [String]
arbitraryLang r = nub <$> (vector =<< choose r)

-- | Custom language generation.
newtype Lang = Lang [String] deriving Show
getWords :: Lang -> [String]
getWords (Lang xs) = xs
instance Arbitrary Lang where
    arbitrary = Lang <$> arbitraryLang langRange

-- | QuickCheck property: set of matching dictionary entries should
-- be the same no matter which searching function is used.
propConsistency :: CostDesc -> Positive Val -> String -> Lang -> Bool
propConsistency costDesc kP xR lang = eq
    [ nub (B.search cost k x ys)
    , nub (S.search cost k x trie)
    , nub (F.search cost k x trie) ]
  where
    x = fromString xR
    eq xs = and [x == x' | (x, x') <- zip xs (tail xs)] 
    cost = fromDesc costDesc
    k = getPositive kP
    trie = C.fromLang (getWords lang) :: T.Trie (Maybe ())
    ys = [(fromString y, ()) | y <- getWords lang]

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList
