import System.Random (getStdGen)
import Control.Applicative ((<$>))
import Criterion.Main
import Test.QuickCheck
import Test.QuickCheck.Gen

import qualified Data.Adict.Slow as S
import qualified Data.Adict.Fast as F
import qualified Data.Adict.Check as C
import qualified Data.Trie as T
import qualified Data.Trie.Class as T hiding (Trie)
import Data.Adict.Base

benchmarkSearch :: Cost Char -> Double -> T.Trie (Maybe ())
                -> Word -> Benchmark
benchmarkSearch cost k trie x = bgroup ("Search-" ++ desc)
    [ bench "Opt"  $ whnf (length . flip (F.search cost k) trie) x
    , bench "Base" $ whnf (length . flip (S.search cost k) trie) x ]
  where
    desc = "(" ++ show (T.size trie) ++ ", " ++ show (wordSize x) ++ ")"

string :: Gen String
string = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z']

arbitraryPoint :: Int -> Int -> Gen (T.Trie (Maybe ()), Word)
arbitraryPoint n k = do
    trie <- T.fromLang <$> vector n
    x    <- fromString <$> vector k
    return (trie, x)

get :: Gen a -> IO a
get (MkGen m) = do
    rnd <- getStdGen
    return (m rnd 10)

main = do
    let ns = [100000, 1000000]
    let ks = [5, 10, 15, 20]

    xs <- sequence
        [ get $ arbitraryPoint n k
        | n <- ns, k <- ks ]

    let cost = costDefault
    let th   = 1.0
    -- cost <- C.fromDesc <$> get arbitrary    -- ^ Arbitrary cost function
    -- th  <- getPositive <$> get arbitrary    -- ^ Threshold

    defaultMain
        [ benchmarkSearch cost th lang x
        | (lang, x) <- xs ]
