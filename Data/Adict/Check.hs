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

-- | Helper structure with Arbitrary instance (implementation below),
-- which can be transformed to an Adict Cost function.
data CostDesc a = CostDesc
    { insD :: M.Map (P a) Double
    , delD :: M.Map (P a) Double
    , subD :: M.Map (P a, P a) Double }
    deriving Show

-- | Construct Cost function from a description structure.
fromDesc :: Ord a => CostDesc a -> Cost a
fromDesc CostDesc{..} = Cost ins del sub
  where
    ins x = fromJust $ x `M.lookup` insD <|> return 1
    del x = fromJust $ x `M.lookup` delD <|> return 1
    sub x y = fromJust $ (x, y) `M.lookup` subD  <|> return (sub' x y)
    sub' (_, x) (_, y)
        | x == y    = 0
        | otherwise = 1

-- | Newtypes for (Pos, a) pair and Cost values, so that we can get
-- custom Arbitrary instances for them.
newtype PP a = PP { unPP :: (Pos, a) }
newtype Val  = Val { unVal :: Double }

instance Arbitrary a => Arbitrary (PP a) where
    arbitrary = do
        pos <- choose posRange
        x   <- arbitrary
        return $ PP (pos, x)

instance Arbitrary Val where
    arbitrary = Val <$> choose valRange

instance (Ord a, Arbitrary a) => Arbitrary (CostDesc a) where
    arbitrary = do
        ins <- mkIns <$> arbitraryList
        del <- mkDel <$> arbitraryList
        sub <- mkSub <$> arbitraryList
        return $ CostDesc ins del sub
      where 
        arbitraryList :: Arbitrary a => Gen [a]
        arbitraryList = choose descRange >>= vector

        mkIns = mkSmp
        mkDel = mkSmp
        mkSmp = M.fromList . map (\(x, v) -> (unPP x, unVal v))
        mkSub = M.fromList . map (\(x, y, v) -> ((unPP x, unPP y), unVal v))

-- | Custom language generation.
newtype Lang = Lang [String] deriving Show
getWords :: Lang -> [String]
getWords (Lang xs) = xs
instance Arbitrary Lang where
    arbitrary = Lang . nub <$> (vector =<< choose langRange)

-- | QuickCheck property: set of matching dictionary entries should
-- be the same no matter which searching function (simpleSearch or
-- optimized levenSearch) is used.
propConsistency :: CostDesc Char -> Positive Double
                -> String -> Lang -> Bool
propConsistency costDesc kP x lang =
    nub (simpleSearch cost k x ys) == nub (levenSearch cost k x trie)
  where
    cost = fromDesc costDesc
    k = getPositive kP
    ys = [(y, ()) | y <- getWords lang]
    trie = fromList ys

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList
