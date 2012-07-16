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

import qualified Text.PoliMorf as Poli
import qualified Text.Prolexbase as Prolex
import qualified Text.LMF.Named as Named

import Data.Trie.Trie (Trie)
import Data.Trie.Class (fromList)
import Data.Map.Combine (poliWith, RelCode(..))
import qualified Data.DAWG.Trie as D

instance Binary T.Text where
    put = put . T.encodeUtf8
    get = T.decodeUtf8 <$> get 

-- | Make label map from LMF.
mkLabelMap :: [(T.Text, T.Text)] -> M.Map T.Text [T.Text]
mkLabelMap xs
    = fmap S.toList
    $ fromListWith S.union
    $ concatMap process xs
  where
    -- | Key k can be a multiword NE.
    process (k, x)
        | length (T.words k) > 1 = []
        | otherwise = [(k, S.singleton x)]

-- -- | Make label map from LMF.
-- mkLabelMap :: [(T.Text, T.Text)] -> M.Map T.Text [T.Text]
-- mkLabelMap xs
--     = fmap S.toList
--     $ fromListWith S.union
--     $ concatMap process xs
--   where
--     -- | Key k can be a multiword NE.
--     process (k, x) =
--         [(k, label) | k <- ks]
--       where
--         ks = T.words k
--         label = S.singleton $ if length ks == 1
--             then "e-" `T.append` x
--             else "p-" `T.append` x

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> M.Map k a
fromListWith f xs =
    let update m (!k, !x) = M.insertWith' f k x m
    in  foldl' update M.empty xs

----- Main program -----

main = do
    [poliPath, lmfPath, wikiPath, outPath] <- getArgs

    poliEs <- Poli.parsePoliMorf <$> readFile poliPath
    let poli = M.fromList [(Poli.form x, Poli.base x) | x <- poliEs]

    lmfEs  <- Named.parseNamed <$> readFile lmfPath
    let lmf = mkLabelMap [(Named.neOrth x, Named.neType x) | x <- lmfEs]

    wikiEs <- Prolex.parseProlexbase <$> readFile wikiPath
    let wiki = mkLabelMap [(Prolex.neOrth x, Prolex.neType x) | x <- wikiEs]

    let ne = M.unionWith (++) lmf wiki
    let poliNe = join . maybeToList
             <$> fmap unRel
             <$> poliWith poli ne
    let xs = [(T.unpack x, y) | (x, y) <- M.toList poliNe]
    let dawg = D.mkDAWG (fromList xs :: Trie (Maybe [T.Text]))
    encodeFile outPath dawg
  where
    unRel (xs, code) = map (T.pack (show code) `T.append`) xs
