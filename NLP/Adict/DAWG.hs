{-# LANGUAGE RecordWildCards #-}

module NLP.Adict.DAWG
( DAWGD
, DAWG (..)
, fromTrie
, fromDAWG

, size
, row
, Row (..)
, entry
, charOn
, valueIn
, edges
, edgeOn

, serialize
, deserialize
) where

import Control.Applicative ((<$>))
import Data.Maybe (listToMaybe)
import Data.Binary (Binary, get, put)
import qualified Data.Vector as V

import NLP.Adict.DAWG.Node
import qualified NLP.Adict.Trie as Trie

-- | A DAWGD dictionary is a DAWG which may have 'Nothing' values along the
-- paths from the root to leaves.
type DAWGD a b = DAWG a (Maybe b)

-- | A directed acyclic word graph with character type @a and dictionary
-- entry type @b.
data DAWG a b = DAWG
    { root  :: Int
    , array :: V.Vector (Row a b) }

-- | Find and eliminate all common subtries in the input trie
-- and return the trie represented as a DAWG.
fromTrie :: (Ord a, Ord b) => Trie.Trie a b -> DAWG a b
fromTrie = deserialize . Trie.serialize

fromDAWG :: Ord a => DAWG a b -> Trie.Trie a b
fromDAWG = Trie.deserialize . serialize

size :: DAWG a b -> Int
size = V.length . array
{-# INLINE size #-}

row :: DAWG a b -> Int -> Row a b
row dag k = array dag V.! k
{-# INLINE row #-}

data Row a b = Row
    { rowValue :: b
    , rowEdges :: V.Vector (a, Int) }

valueIn :: DAWG a b -> Int -> b
valueIn dag k = rowValue (array dag V.! k)
{-# INLINE valueIn #-}

edges :: DAWG a b -> Int -> [(a, Int)]
edges dag k = V.toList . rowEdges $ row dag k
{-# INLINE edges #-}

edgeOn :: Eq a => DAWG a b -> Int -> a -> Maybe Int
edgeOn DAWG{..} k x =
    let r = array V.! k
    in  snd <$> V.find ((x==).fst) (rowEdges r)

entry :: DAWG a (Maybe b) -> [Int] -> Maybe ([a], b)
entry dag xs = do
    x <- mapM (charOn dag) (zip (root dag:xs) xs)
    r <- maybeLast xs >>= valueIn dag 
    return (x, r)
  where
    maybeLast [] = Nothing
    maybeLast ys = Just $ last ys

charOn :: DAWG a b -> (Int, Int) -> Maybe a
charOn dag (root, x) = listToMaybe
    [c | (c, y) <- edges dag root, x == y]

serialize :: Ord a => DAWG a b -> [Node a b]
serialize = map unRow . V.toList . array

-- | Assumptiom: root node is last in the serialization list.
deserialize :: Ord a => [Node a b] -> DAWG a b
deserialize xs =
    let arr = V.fromList $ map mkRow xs
    in  DAWG (V.length arr - 1) arr

unRow :: Ord a => Row a b -> Node a b
unRow Row{..} = mkNode rowValue (V.toList rowEdges)
{-# INLINE unRow #-}

mkRow :: Ord a => Node a b -> Row a b
mkRow n = Row (nodeValue n) (V.fromList $ nodeEdges n)
{-# INLINE mkRow #-}

instance (Ord a, Binary a, Binary b) => Binary (DAWG a b) where
    put = put . serialize
    get = deserialize <$> get

-- goDown :: DAWG a -> Int -> DAWG a
-- goDown DAWG{..} k = DAWG k array
-- 
-- instance T.Trie DAWGArray where
--     unTrie dag@DAWGArray{..} =
--         let row = array V.! root
--         in  ( valueIn row
--             , [ (c, goDown dag k)
--               | (c, k) <- U.toList (edgeVec row) ] )
--     child x dag@DAWGArray{..} =
--         let row = array V.! root
--         in  goDown dag <$> snd <$> U.find ((x==).fst) (edgeVec row)
