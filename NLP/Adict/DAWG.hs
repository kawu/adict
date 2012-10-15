{-# LANGUAGE RecordWildCards #-}

-- | A directed acyclic word graph.

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
) where

import Control.Applicative ((<$>))
import Data.Maybe (listToMaybe)
import Data.Binary (Binary, get, put)
import qualified Data.Vector as V

import NLP.Adict.Node
import qualified NLP.Adict.Trie as Trie
import qualified NLP.Adict.Trie.Serialize as Trie

-- | A DAWGD dictionary is a 'DAWG' which may have the 'Nothing' value
-- along the path from the root to a leaf.
type DAWGD a b = DAWG a (Maybe b)

-- | A directed acyclic word graph with character type @a@ and dictionary
-- entry type @b@.  Each node is represented by a unique integer number
-- which is also an index of the node in the DAWG array.
data DAWG a b = DAWG
    { root  :: Int                  -- ^ Root (index) of the DAWG
    , array :: V.Vector (Row a b)   -- ^ Vector of DAWG nodes
    }

-- | Find and eliminate all common subtries in the input trie
-- and return the trie represented as a DAWG.
fromTrie :: (Ord a, Ord b) => Trie.Trie a b -> DAWG a b
fromTrie = deserialize . Trie.serialize

-- | Transform the DAWG to implicit DAWG in a form of a trie.
fromDAWG :: Ord a => DAWG a b -> Trie.Trie a b
fromDAWG = Trie.deserialize . serialize

-- | Size of the DAWG.
size :: DAWG a b -> Int
size = V.length . array
{-# INLINE size #-}

-- | Row (node) of the DAWG on the given array position.
row :: DAWG a b -> Int -> Row a b
row dag k = array dag V.! k
{-# INLINE row #-}

-- | A Row represents one node of the DAWG.
data Row a b = Row {
    -- | Value in the node.
    rowValue :: b, 
    -- | Edges to subnodes (represented by DAWG array indices)
    -- annotated with characters.
    rowEdges :: V.Vector (a, Int)
    }

-- | Value in the DAWG node represented by the index.
valueIn :: DAWG a b -> Int -> b
valueIn dag k = rowValue (array dag V.! k)
{-# INLINE valueIn #-}

-- | Edges starting from the DAWG node represented by the index.
edges :: DAWG a b -> Int -> [(a, Int)]
edges dag k = V.toList . rowEdges $ row dag k
{-# INLINE edges #-}

-- | Index of the node following the edge annotated with the
-- given character.
edgeOn :: Eq a => DAWG a b -> Int -> a -> Maybe Int
edgeOn DAWG{..} k x =
    let r = array V.! k
    in  snd <$> V.find ((x==).fst) (rowEdges r)

-- | Return the dictionary entry determined by following the
-- path of node indices.
entry :: DAWG a (Maybe b) -> [Int] -> Maybe ([a], b)
entry dag xs = do
    x <- mapM (charOn dag) (zip (root dag:xs) xs)
    r <- maybeLast xs >>= valueIn dag 
    return (x, r)
  where
    maybeLast [] = Nothing
    maybeLast ys = Just $ last ys

-- | Determine the character on the edges between two nodes.
charOn :: DAWG a b -> (Int, Int) -> Maybe a
charOn dag (root, x) = listToMaybe
    [c | (c, y) <- edges dag root, x == y]

-- | Serialize the DAWG into a list of nodes.
serialize :: Ord a => DAWG a b -> [Node a b]
serialize = map unRow . V.toList . array

-- | Deserialize the DAWG from a list of nodes.  Assumptiom: root node
-- is last in the serialization list.
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
