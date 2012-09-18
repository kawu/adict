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


instance D.DAWG DAWGArray where
    root    DAWGArray{..}   = root
    valueIn DAWGArray{..} k = valueIn (array V.! k)
    edges   DAWGArray{..} k = U.toList $ edgeVec $ array V.! k
    edgeOn dag@DAWGArray{..} k x =
        let row = array V.! k
        in  snd <$> U.find ((x==).fst) (edgeVec row)
