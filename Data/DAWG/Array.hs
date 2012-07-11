{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.DAWG.Array
( DAWGArray (..)
, DAWGRow (..)
, size
, row
, edges
) where

import Control.Applicative ((<$>))
import Data.Maybe (listToMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Binary (Binary, get, put)

import qualified Data.Trie.Class as T
import qualified Data.DAWG.Class as D
import qualified Data.DAWG.Node as N

data DAWGArray a = DAWGArray
    { root  :: Int
    , array :: V.Vector (DAWGRow a) }

size :: DAWGArray a -> Int
size = V.length . array

row :: DAWGArray a -> Int -> DAWGRow a
row dag k = array dag V.! k

data DAWGRow a  = DAWGRow
    { valueIn :: a
    , edgeVec :: U.Vector (Char, Int) }

-- entry :: DAWGArray (Maybe a) -> [Int] -> Maybe (String, a)
-- entry dag xs = do
--     x <- mapM (charOn dag) (zip (root dag:xs) xs)
--     r <- T.lookup x dag
--     return (x, r)
-- 
-- charOn :: DAWGArray a -> (Int, Int) -> Maybe Char
-- charOn dag (root, x) = listToMaybe
--     [c | (c, y) <- edges dag root, x == y]

serialize :: Ord a => DAWGArray a -> [N.Node a]
serialize = map unRow . V.toList . array

-- | Assumptiom: root node is last in the serialization list.
deserialize :: Ord a => [N.Node a] -> DAWGArray a
deserialize xs =
    let arr = V.fromList $ map mkRow xs
    in  DAWGArray (V.length arr - 1) arr

unRow :: Ord a => DAWGRow a -> N.Node a
unRow DAWGRow{..} = N.mkNode valueIn (U.toList edgeVec)

mkRow :: Ord a => N.Node a -> DAWGRow a
mkRow n = DAWGRow (N.nodeValue n) (U.fromList $ N.nodeEdges n)

instance (Ord a, Binary a) => Binary (DAWGArray a) where
    put = put . serialize
    get = deserialize <$> get

goDown :: DAWGArray a -> Int -> DAWGArray a
goDown DAWGArray{..} k = DAWGArray k array

edges :: DAWGArray a -> Int -> [(Char, Int)]
edges dag k =
    U.toList $ edgeVec (row dag k)

instance D.DAWG DAWGArray where
    root    DAWGArray{..}   = root
    valueIn DAWGArray{..} k = valueIn (array V.! k)
    edges   DAWGArray{..} k = U.toList $ edgeVec $ array V.! k
    edgeOn dag@DAWGArray{..} k x =
        let row = array V.! k
        in  snd <$> U.find ((x==).fst) (edgeVec row)

instance T.Trie DAWGArray where
    unTrie dag@DAWGArray{..} =
        let row = array V.! root
        in  ( valueIn row
            , [ (c, goDown dag k)
              | (c, k) <- U.toList (edgeVec row) ] )
    child x dag@DAWGArray{..} =
        let row = array V.! root
        in  goDown dag <$> snd <$> U.find ((x==).fst) (edgeVec row)
