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

import Text.XML.PolySoup

import Data.Trie.Trie (Trie)
import Data.Trie.Class (fromList)
import Data.Map.Combine (poliHist, RelCode(..))
import qualified Data.DAWG.Trie as D

instance Binary T.Text where
    put = put . T.encodeUtf8
    get = T.decodeUtf8 <$> get 

----- PoliMorf parser -----

type Form  = T.Text
type Lemma = T.Text

-- | Get a list of pairs (form, lemma) from a PoliMorf string.
parsePoliMorf :: String -> [(Form, Lemma)]
parsePoliMorf = map parsePoliRow . lines 

-- | Get a (form, lemma) pair from a PoliMorf row.
parsePoliRow :: String -> (Form, Lemma)
parsePoliRow row =
    let xs = break (=='\t') row
        x  = T.pack (fst xs)
        y  = T.pack (snd xs)
    in  x `seq` y `seq` (x, y)

----- LMF parser -----

type ID = T.Text

lmfP :: XmlParser String [(Lemma, ID)]
lmfP = true //> lexEntryP

lexEntryP :: XmlParser String (Lemma, ID)
lexEntryP = (tag "LexicalEntry" *> getAttr "id") `join` \lexId -> do
    many_ $ cut $ tag "feat"
    -- xs <- many wordP
    lemma <- lemmaP <* ignore
    return (lemma, T.pack lexId)

lemmaP :: XmlParser String Lemma
lemmaP = T.pack . head <$> (tag "Lemma" //> featP "writtenForm")

-- wordP :: XmlParser String String
-- wordP = head <$> (tag "Lemma" <|> tag "WordForm" //> featP "writtenForm")

featP :: String -> XmlParser String String
featP att = cut (tag "feat" *> hasAttr "att" att *> getAttr "val")

----- Main program -----

type IdTrie = Trie (Maybe (Maybe (ID, RelCode)))

main = do
    [poliPath, lmfPath, outPath] <- getArgs
    poli <- M.fromList . parsePoliMorf <$> readFile poliPath
    lmf  <- M.fromList . parseXML lmfP <$> readFile lmfPath
    let poliNe = poliHist poli lmf
    let xs = [(T.unpack x, y) | (x, y) <- M.toList poliNe]
    let dawg = D.mkDAWG (fromList xs :: IdTrie)
    encodeFile outPath dawg
