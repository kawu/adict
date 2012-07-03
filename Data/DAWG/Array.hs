{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.DAWG.Array
( DAWGArray (..)
) where

import Control.Applicative ((<$>))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Binary (Binary, get, put)

import qualified Data.Trie.Class as C
import qualified Data.DAWG.Node as N

data DAWGArray a = DAWGArray
    { root  :: Int
    , array :: V.Vector (DAWGRow a) }

data DAWGRow a  = DAWGRow
    { valueIn :: a
    , edgeVec :: U.Vector (Char, Int) }

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

instance C.Trie DAWGArray where
    unTrie dag@DAWGArray{..} =
        let row = array V.! root
        in  ( valueIn row
            , [ (c, goDown dag k)
              | (c, k) <- U.toList (edgeVec row) ] )
    child x dag@DAWGArray{..} =
        let row = array V.! root
        in  goDown dag <$> snd <$> U.find ((x==).fst) (edgeVec row)
                
