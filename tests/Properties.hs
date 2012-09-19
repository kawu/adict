{-# LANGUAGE RecordWildCards #-}

-- | QuickCheck properties which should be satisfied by the Adict.

import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe (fromJust)

import Test.QuickCheck hiding (listOf)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import NLP.Adict.Core
import qualified NLP.Adict.Brute as Br
import qualified NLP.Adict.Basic as Ba
import qualified NLP.Adict.Trie as Trie

-- | Check parameters.
posRange :: (Int, Int)
posRange  = (0, 4)	-- ^ Position of random edit op

valRange :: (Val, Val)
valRange  = (0, 10)	-- ^ Weight of edit op 

descRange :: (Int, Int)
descRange = (0, 25)	-- ^ Number of random edit ops of given type

langRange :: (Int, Int)
langRange = (0, 25)	-- ^ Size of language

type Val = Double

-- | Helper structure with Arbitrary instance (implementation below),
-- which can be transformed to the Adict Cost function.
data CostDesc = CostDesc
    { insD :: M.Map (Pos, Char) Val
    , delD :: M.Map (Pos, Char) Val
    , subD :: M.Map (Pos, Char, Char) Val }
    deriving Show

-- | Construct Cost function from a description structure.
toCost :: CostDesc -> Cost Char
toCost CostDesc{..} = Cost ins del sub
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
propConsistency costDesc kP xR lang =
    let br = (nub . map unWord) (Br.search cost k x ys)
        ba = nub (Ba.search cost k x trie)
    in  br == ba
  where
    x = V.fromList xR
    cost = toCost costDesc
    k = getPositive kP
    trie = Trie.fromLang (getWords lang)
    ys = [(V.fromList y, ()) | y <- getWords lang]
    unWord (word, v, w) = (V.toList word, v, w)

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [testProperty "consistency" propConsistency]
