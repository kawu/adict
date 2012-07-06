{-# LANGUAGE PackageImports #-}

import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Data.List (break)
import Data.Binary (encodeFile)

import qualified "adict" Data.Trie as T
import Data.Trie.Class (fromLang)
import qualified Data.DAWG as D

-- | Get a list of forms from PoliMorf string.
parsePoliMorf :: String -> [String]
parsePoliMorf = map parsePoliRow . lines 

-- | Get a form from a PoliMorf row.
parsePoliRow :: String -> String
parsePoliRow = fst . break (=='\t')

main = do
    [inPath, outPath] <- getArgs
    xs <- parsePoliMorf <$> readFile inPath
    let dawg = D.mkDAWG (fromLang xs :: T.Trie (Maybe ()))
    encodeFile outPath dawg
