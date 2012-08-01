{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs)
import Control.Applicative ((<$>))
import qualified Control.Monad as Monad
import Data.List (break, foldl')
import Data.Maybe (maybeToList)
import Data.Binary (Binary, encodeFile, put, get)

import qualified Data.Map as M
import qualified Data.Set as S

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import qualified Data.PoliMorf as Poli
import qualified Text.PoliMorf as Poli
import qualified Text.LMF.Hist as Hist

import Data.Trie.Trie (Trie)
import Data.Trie.Class (fromList)
import qualified Data.DAWG.Trie as D

instance Binary T.Text where
    put = put . T.encodeUtf8
    get = T.decodeUtf8 <$> get 

type ID     = T.Text
type IdTrie = Trie (Maybe (Maybe (ID, Poli.RelCode)))

main = do
    [poliPath, histPath, outPath] <- getArgs

    poliEs <- Poli.parsePoliMorf <$> readFile poliPath
    let poli = M.fromList [(Poli.form x, Poli.base x) | x <- poliEs]

    histEs  <- Hist.parseHist <$> readFile histPath
    let hist = M.fromList [(Hist.base x, Hist.lxId x) | x <- histEs]

    let poliHist = poli `Poli.joinHist` hist
    let xs = [(T.unpack x, y) | (x, y) <- M.toList poliHist]
    let dawg = D.mkDAWG (fromList xs :: IdTrie)
    encodeFile outPath dawg
