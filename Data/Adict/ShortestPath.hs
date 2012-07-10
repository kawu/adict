module Data.Adict.ShortestPath
( search
) where

import Data.Maybe (isJust, maybeToList)
import Data.List (group)

import Data.Adict.Base
import Data.DAWG.Class
import Data.Graph.ShortestPath

type NodeID  = Int
data Node = N {-# UNPACK #-} !NodeID {-# UNPACK #-} !Pos
    deriving (Show, Eq, Ord)

{-# INLINE nodeId #-}
nodeId :: Node -> NodeID
nodeId (N x _) = x

type Dag d a = d (Maybe a)

search :: DAWG d => Cost -> Double -> Word -> Dag d a -> Maybe (Node, Double)
search cost z x dag =
    -- entry dag . nub . reverse . map nodeId . P.path =<<
    minPath z edgesFrom isEnd (N (root dag) 0)
  where
    -- nub = map head . group
    edgesFrom (N n i) =
        skew ++ down ++ right
      where
        j = i+1
        down
            | j > wordLength x = []
            | otherwise =
	    	[(N n j, (delete cost) j (x#j))]
        right = 
            [ (N m i, (insert cost) j c)
            | (c, m) <- edges dag n ]
        skew
            | j > wordLength x = []
            | otherwise =
                [ (N m j, 0)
                | m <- maybeToList $ edgeOn dag n (x#j) ]
                ++
                [ (N m j, (subst cost)  j (x#j) c)
                | (c, m) <- edges dag n, x#j /= c ]
    isEnd (N n k) = k == wordLength x
                 && isJust (valueIn dag n)
