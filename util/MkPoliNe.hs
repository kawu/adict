{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.List (break, foldl')
import Data.Maybe (maybeToList)
import Data.Binary (Binary, encodeFile, put, get)

import qualified Data.Map as M
import qualified Data.Set as S

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Text.XML.PolySoup hiding (join)

import Data.Trie.Trie (Trie)
import Data.Trie.Class (fromList)
import Data.Map.Combine (poliWith)
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

type NE    = T.Text
type Type  = T.Text
type Label = T.Text

lmfP :: XmlParser String [(NE, Type)]
lmfP = true ##> lexEntryP

lexEntryP :: XmlParser String [(NE, Type)]
lexEntryP = tag "LexicalEntry" `joinR` do
    many_ $ cut $ tag "feat"
    words <- many wordP
    !sense <- senseP
    return [(x, sense) | !x <- words]

wordP :: XmlParser String NE
wordP = head <$> (tag "Lemma" <|> tag "WordForm" /> featP "writtenForm")

senseP :: XmlParser String Type
senseP = head <$> (tag "Sense" //> featP "externalReference" <|> featP "label")

featP :: String -> XmlParser String T.Text
featP att = T.pack <$> cut (tag "feat" *> hasAttr "att" att *> getAttr "val")

-- | Make label map from LMF.
mkLabelMap :: [(NE, Type)] -> M.Map Form [Label]
mkLabelMap xs
    = fmap S.toList
    $ fromListWith S.union
    $ concatMap process xs
  where
    -- | Key k can be a multiword NE.
    process (k, x) =
        [(k, label) | k <- ks]
      where
        ks = T.words k
        label = S.singleton $ if length ks == 1
            then "e-" `T.append` x
            else "p-" `T.append` x

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> M.Map k a
fromListWith f xs =
    let update m (!k, !x) = M.insertWith' f k x m
    in  foldl' update M.empty xs


----- Main program -----

main = do
    [poliPath, nePath, outPath] <- getArgs
    poli <- M.fromList . parsePoliMorf <$> readFile poliPath
    ne   <- mkLabelMap . parseXML lmfP <$> readFile nePath
    let poliNe = join . maybeToList <$> poliWith poli ne
    let xs = [(T.unpack x, y) | (x, y) <- M.toList poliNe]
    let dawg = D.mkDAWG (fromList xs :: Trie (Maybe [Label]))
    encodeFile outPath dawg