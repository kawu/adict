module Data.DAWG.Adict
( search
) where

import Data.Maybe (isJust)
import Data.List (group)

import Data.Adict.Base
import Data.DAWG.Array
-- import Data.Graph.ShortestPath hiding (path)
import Data.Graph.ShortestPath
import qualified Data.Graph.ShortestPath as P

type DAG a = DAWGArray (Maybe a)

type NodeID  = Int
data Node = N {-# UNPACK #-} !NodeID {-# UNPACK #-} !Pos
    deriving (Eq, Ord)

{-# INLINE nodeId #-}
nodeId :: Node -> NodeID
nodeId (N x _) = x

-- search :: Cost Char -> Double -> Word -> DAG a -> Maybe (String, a)
search cost th x dag =
    -- entry dag . nub . reverse . map nodeId . P.path =<<
    minPath th edgesFrom isEnd (N (root dag) 0)
  where
    -- nub = map head . group
    edgesFrom (N n i) =
        down ++ right ++ skew
      where
        j = i+1
        down
            | j > wordLength x = []
            | otherwise = [(N n j, (delete cost) (j, x#j) undefined)]
        right = 
            [ (N m i, (insert cost) j (undefined, c))
            | (c, m) <- edges dag n ]
        skew
            | j > wordLength x = []
            | otherwise =
                [ (N m j, (subst cost)  (j, x#j) (undefined, c))
                | (c, m) <- edges dag n ]
    isEnd (N n k) = k == wordLength x
                 && isJust (valueIn $ row dag n)
