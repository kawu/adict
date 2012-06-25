{-# LANGUAGE RecordWildCards #-}

-- QuickCheck properties, which should be satisfied by Adict.

module Data.Adict.Check
( CostDesc (..)
, fromDesc
, propConsistency
) where

import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Test.QuickCheck

import Data.Adict

-- | Check parameters.
posRange  = (0, 10)	-- ^ Position of random edit op
valRange  = (0, 10)	-- ^ Weight of edit op 
descRange = (0, 1000)	-- ^ Number of random edit ops of given type
langRange = (0, 1000)	-- ^ Size of language

-- | Simple type synonym for (Pos, a).
type P a = (Pos, a)

type Val = Double

-- | Helper structure with Arbitrary instance (implementation below),
-- which can be transformed to an Adict Cost function.
data CostDesc a = CostDesc
    { insD :: M.Map (Pos, P a) Val
    , delD :: M.Map (P a, Pos) Val
    , subD :: M.Map (P a, P a) Val }
    deriving Show

-- | Construct Cost function from a description structure.
fromDesc :: Ord a => CostDesc a -> Cost a
fromDesc CostDesc{..} = Cost ins del sub
  where
    ins i y = fromJust $ (i, y) `M.lookup` insD <|> return 1
    del x j = fromJust $ (x, j) `M.lookup` delD <|> return 1
    sub x y = fromJust $ (x, y) `M.lookup` subD  <|> return (sub' x y)
    sub' (_, x) (_, y)
        | x == y    = 0
        | otherwise = 1

arbitraryPos :: Gen Pos
arbitraryPos = choose posRange

arbitraryVal :: Gen Val
arbitraryVal = choose valRange

arbitraryP :: Arbitrary a => Gen (P a)
arbitraryP = (,) <$> arbitraryPos <*> arbitrary

instance (Ord a, Arbitrary a) => Arbitrary (CostDesc a) where
    arbitrary = do
        ins <- M.fromList <$> listOf insElem 
        del <- M.fromList <$> listOf delElem
        sub <- M.fromList <$> listOf subElem
        return $ CostDesc ins del sub
      where 
	insElem  = (,) <$> arbitraryPos <*> arbitraryP
	delElem  = (,) <$> arbitraryP   <*> arbitraryPos
	subElem  = (,) <$> arbitraryP   <*> arbitraryP
        listOf m = do
            k  <- choose descRange
            vectorOf k ((,) <$> m <*> arbitraryVal)

-- | Custom language generation.
newtype Lang = Lang [String] deriving Show
getWords :: Lang -> [String]
getWords (Lang xs) = xs
instance Arbitrary Lang where
    arbitrary = Lang . nub <$> (vector =<< choose langRange)

-- | QuickCheck property: set of matching dictionary entries should
-- be the same no matter which searching function (simpleSearch or
-- optimized levenSearch) is used.
propConsistency :: CostDesc Char -> Positive Val -> String -> Lang -> Bool
propConsistency costDesc kP x lang =
    nub (simpleSearch cost k x ys) == nub (levenSearch cost k x trie)
  where
    cost = fromDesc costDesc
    k = getPositive kP
    ys = [(y, ()) | y <- getWords lang]
    trie = fromList ys

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList
