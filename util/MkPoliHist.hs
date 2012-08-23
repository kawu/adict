{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs)
import Control.Applicative ((<$>))
import qualified Control.Monad as Monad
import Data.List (break, foldl')
import Data.Maybe (maybeToList)
import Data.Binary (Binary, encodeFile, put, get)
import Data.Text.Binary

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

type ID     = T.Text
type IdTrie = Trie (Maybe (Maybe ([ID], Poli.RelCode)))

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> M.Map k a
fromListWith f xs =
    let update m (!k, !x) = M.insertWith' f k x m
    in  foldl' update M.empty xs

main = do
    [poliPath, histPath, outPath] <- getArgs

    poliEs <- Poli.parsePoliMorf <$> readFile poliPath
    let poli = fromListWith (++)
            [ (Poli.form x, [Poli.base x])
            | x <- poliEs ]

    histEs  <- Hist.parseHist <$> readFile histPath
    let hist = fmap S.fromList $ fromListWith (++)
            [ (form, [Hist.lxId x])
            | x <- histEs
            , form <- Hist.forms x ]

    let poliHist = Poli.joinDict poli hist
    let unSet x = let f (s, y) = (S.toList s, y) in fmap f x
    let xs = [(T.unpack x, unSet y) | (x, y) <- M.toList poliHist]
    let dawg = D.mkDAWG (fromList xs :: IdTrie)
    encodeFile outPath dawg
