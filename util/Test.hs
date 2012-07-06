import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf
import Data.Binary (decodeFile)
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U

import Data.DAWG.Array
import Data.DAWG.Adict (search)
import Data.Adict.Base (costDefault)

type Dict = DAWGArray (Maybe ())

onInput :: Dict -> String -> String
onInput dict = unlines . map (onLine dict) . lines

onLine  :: Dict -> String -> String
onLine dict line =
    let [x, k] = words line
        y = search costDefault (read k) (U.fromList x) dict
    in  show y

-- onLine  :: Dict -> String -> String
-- onLine dict line =
--     let [x, k] = words line
--         thMod = thresMargin 0.1 0.25
--         (xs, log) = runAdict costDefault (read k) (U.fromList x)
--                             (search thMod dict)
--     in  log ++ "RESULT:\n" ++ intercalate "\n" (map show xs) ++ "\n"

main = do
    [inPath] <- getArgs
    dict <- decodeFile inPath :: IO Dict
    putStrLn $ "size: " ++ show (size dict)
    -- interact (onInput dict)
    beg <- getCPUTime
    mapM_ (putStrLn . onInput dict)
	  (replicate 10000 "kuternoga 1")
    end <- getCPUTime
    let diff = (fromIntegral (end - beg)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
