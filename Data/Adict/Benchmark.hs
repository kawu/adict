import System.Random (getStdGen)
import Control.Applicative ((<$>))
import Criterion.Main
import Test.QuickCheck
import Test.QuickCheck.Gen
import qualified Data.Vector.Unboxed as U

import qualified Data.Adict as A
import qualified Data.Adict.Fast as F
import qualified Data.Adict.Check as C

type Word = U.Vector Char

benchmarkSearch :: A.Cost Char -> Double -> A.Trie Char ()
                -> Word -> Benchmark
benchmarkSearch cost k trie x = bgroup ("Search-" ++ desc)
    [ bench "Opt"  $ whnf (length . flip (F.levenSearch cost k) trie) x
    , bench "Base" $ whnf (length . flip (A.levenSearch cost k) trie) x ]
  where
    desc = "(" ++ show (A.size trie) ++ ", " ++ show (U.length x) ++ ")"

string :: Gen String
string = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z']

arbitraryPoint :: Int -> Int -> Gen (A.Trie Char (), Word)
arbitraryPoint n k = do
    trie <- A.fromLang <$> vector n
    x    <- U.fromList <$> vector k
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

    let cost = A.costDefault
    let th   = 1.0
    -- cost <- C.fromDesc <$> get arbitrary    -- ^ Arbitrary cost function
    -- th  <- getPositive <$> get arbitrary    -- ^ Threshold

    defaultMain
        [ benchmarkSearch cost th lang x
        | (lang, x) <- xs ]
