{-# LANGUAGE RecordWildCards #-}

-- | QuickCheck properties which should be satisfied by the Adict.

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V

import Test.QuickCheck
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import NLP.Adict
import qualified NLP.Adict.CostDiv as C
import qualified NLP.Adict.Trie as Trie
import qualified NLP.Adict.DAWG as DAWG

-- | Check parameters.
posRange :: (Int, Int)
posRange  = (0, 4)	-- ^ Position of random edit op

weightRange :: (Weight, Weight)
weightRange  = (0, 10)	-- ^ Weight of edit op 

posModRange :: (Double, Double)
posModRange  = (0.1, 2)	-- ^ Position modifier

descRange :: (Int, Int)
descRange = (0, 25)	-- ^ Number of random edit ops of given type

langRange :: (Int, Int)
langRange = (0, 25)	-- ^ Size of language

arbitraryPos :: Gen Pos
arbitraryPos = choose posRange

arbitraryPosMod :: Gen Double
arbitraryPosMod = choose posModRange

arbitraryWeight :: Gen Weight
arbitraryWeight = choose weightRange

arbitraryChar :: Gen Char
arbitraryChar = elements ['a'..'z']

arbitraryWord :: Gen String
arbitraryWord = listOf arbitraryChar

arbitraryLang :: (Int, Int) -> Gen [String]
arbitraryLang r = nub <$> (flip vectorOf arbitraryWord =<< choose r)

-- | Helper structure with Arbitrary instance (implementation below),
-- which can be transformed to the Adict Cost function.
data CostDesc = CostDesc
    { insD :: M.Map (Pos, Char) Weight
    , delD :: M.Map (Pos, Char) Weight
    , subD :: M.Map (Pos, Char, Char) Weight }
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

instance Arbitrary CostDesc where
    arbitrary = do
        ins <- M.fromList <$> mkList insElem 
        del <- M.fromList <$> mkList delElem
        sub <- M.fromList <$> mkList subElem
        return $ CostDesc ins del sub
      where 
	insElem  = (,)  <$> arbitraryPos <*> arbitraryChar
	delElem  = (,)  <$> arbitraryPos <*> arbitraryChar
	subElem  = (,,) <$> arbitraryPos <*> arbitraryChar <*> arbitraryChar
        mkList m = do
            k <- choose descRange
            vectorOf k ((,) <$> m <*> arbitraryWeight)

-- | Helper structure with Arbitrary instance,
-- which can be transformed to the Adict Cost function.
data CostDivDesc = CostDivDesc
    { insDivD :: [(Char, Weight)]
    , delDivD :: [(Char, Weight)]
    , subDivD :: [(Char, Char, Weight)]
    , posModD :: M.Map Pos Double }
    deriving Show

instance Arbitrary CostDivDesc where
    arbitrary = do
        ins <- mkList insElem 
        del <- mkList delElem
        sub <- mkList subElem
        posMod <- M.fromList <$> mkList posElem
        return $ CostDivDesc ins del sub posMod
      where 
	insElem  = (,)  <$> arbitraryChar <*> arbitraryWeight
	delElem  = (,)  <$> arbitraryChar <*> arbitraryWeight
	subElem  = (,,) <$> arbitraryChar <*> arbitraryChar <*> arbitraryWeight
	posElem  = (,)  <$> arbitraryPos <*> arbitraryPosMod
        mkList m = do
            k <- choose descRange
            vectorOf k m

-- | Construct Cost function from a description structure.
toCostDiv :: CostDivDesc -> C.CostDiv Char
toCostDiv CostDivDesc{..} = C.CostDiv ins del sub posMod
  where
    delMap = M.fromList delDivD
    subMap = C.mkSubMap subDivD
    ins = C.unSub . C.mkSub $ insDivD
    del x = fromJust $ x `M.lookup` delMap <|> pure 1
    sub x
        =  C.Filter (x==) 0
        :  C.unSub (C.subOn x subMap)
        ++ [C.Filter (const True) 1]
    posMod k = fromJust $ k `M.lookup` posModD <|> pure 1

-- | Custom language generation.
newtype Lang = Lang [String] deriving Show
getWords :: Lang -> [String]
getWords (Lang xs) = xs
instance Arbitrary Lang where
    arbitrary = Lang <$> arbitraryLang langRange

-- | QuickCheck property1: set of matching dictionary entries should
-- be the same no matter which searching function is used.
pBaseEqBrute :: CostDesc -> Positive Double -> String -> Lang -> Bool
pBaseEqBrute costDesc kP xR lang =
    let br = (nub . map unWord) (bruteSearch cost k x ys)
        ba = nub (findAll cost k x trie)
    in  br == ba
  where
    x = V.fromList xR
    cost = toCost costDesc
    k = getPositive kP
    trie = Trie.fromLang (getWords lang)
    ys = [(V.fromList y, ()) | y <- getWords lang]
    unWord (word, v, w) = (V.toList word, v, w)

pBaseEqNearest :: CostDivDesc -> Positive Double -> String -> Lang -> Bool
pBaseEqNearest costDesc kP xR lang =
    let ba = findAll cost k x trie
        nr = findNearest costDiv k x dawg
    in  check ba nr
  where
    check [] (Just _) = False
    check [] Nothing  = True
    check ys (Just y) = y `elem`
        ( let thd (_, _, c) = c
              m = minimum . map thd $ ys
          in  filter ((<=m) . thd) ys )
    check _  _        = False

    x = V.fromList xR
    k = getPositive kP

    costDiv = toCostDiv costDesc
    cost = C.toCostInf costDiv

    trie = Trie.fromLang (getWords lang)
    dawg = DAWG.fromTrie trie

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testProperty "brute force == basic" pBaseEqBrute
    , testProperty "nearest == minimum basic" pBaseEqNearest ]
