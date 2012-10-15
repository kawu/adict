{-# LANGUAGE RecordWildCards #-}

-- | A directed acyclic word graph.

module NLP.Adict.DAWG.Internal
( DAWG (..)
, DAWGM
, fromTrie
, fromDAWG

, size
, nodeBy
, Node (..)
, entry
, charOn
, valueBy
, edges
, edgeOn
) where

import Control.Applicative ((<$>))
import Data.Maybe (listToMaybe)
import Data.Binary (Binary, get, put)
import qualified Data.Vector as V

import qualified NLP.Adict.Node as N
import qualified NLP.Adict.Trie.Internal as Trie

-- | A DAWGM is a 'DAWG' with 'Maybe' values in nodes.
type DAWGM a b = DAWG a (Maybe b)

-- | A directed acyclic word graph with character type @a@ and dictionary
-- entry type @b@.  Each node is represented by a unique integer number
-- which is also an index of the node in the vector of DAWG nodes.
data DAWG a b = DAWG
    { root  :: Int                  -- ^ Root (index) of the DAWG
    , nodes :: V.Vector (Node a b)  -- ^ Vector of DAWG nodes
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
size = V.length . nodes
{-# INLINE size #-}

-- | Node by index.
nodeBy :: DAWG a b -> Int -> Node a b
nodeBy dag k = nodes dag V.! k
{-# INLINE nodeBy #-}

-- | A node in the DAWG.
data Node a b = Node {
    -- | Value in the node.
    valueIn  :: b, 
    -- | Edges to subnodes (represented by DAWG node indices)
    -- annotated with characters.
    subNodes :: V.Vector (a, Int)
    }

-- | Value in the DAWG node represented by the index.
valueBy :: DAWG a b -> Int -> b
valueBy dag k = valueIn (nodes dag V.! k)
{-# INLINE valueBy #-}

-- | Edges starting from the DAWG node represented by the index.
edges :: DAWG a b -> Int -> [(a, Int)]
edges dag k = V.toList . subNodes $ nodeBy dag k
{-# INLINE edges #-}

-- | Index of the node following the edge annotated with the
-- given character.
edgeOn :: Eq a => DAWG a b -> Int -> a -> Maybe Int
edgeOn DAWG{..} k x =
    let r = nodes V.! k
    in  snd <$> V.find ((x==).fst) (subNodes r)

-- | Return the dictionary entry determined by following the
-- path of node indices.
entry :: DAWG a (Maybe b) -> [Int] -> Maybe ([a], b)
entry dag xs = do
    x <- mapM (charOn dag) (zip (root dag:xs) xs)
    r <- maybeLast xs >>= valueBy dag 
    return (x, r)
  where
    maybeLast [] = Nothing
    maybeLast ys = Just $ last ys

-- | Determine the character on the edges between two nodes.
charOn :: DAWG a b -> (Int, Int) -> Maybe a
charOn dag (root, x) = listToMaybe
    [c | (c, y) <- edges dag root, x == y]

-- | Serialize the DAWG into a list of nodes.
serialize :: Ord a => DAWG a b -> [N.Node a b]
serialize = map unNode . V.toList . nodes

-- | Deserialize the DAWG from a list of nodes.  Assumptiom: root node
-- is last in the serialization list.
deserialize :: Ord a => [N.Node a b] -> DAWG a b
deserialize xs =
    let arr = V.fromList $ map mkNode xs
    in  DAWG (V.length arr - 1) arr

unNode :: Ord a => Node a b -> N.Node a b
unNode Node{..} = N.mkNode valueIn (V.toList subNodes)
{-# INLINE unNode #-}

mkNode :: Ord a => N.Node a b -> Node a b
mkNode n = Node (N.nodeValue n) (V.fromList $ N.nodeEdges n)
{-# INLINE mkNode #-}

instance (Ord a, Binary a, Binary b) => Binary (DAWG a b) where
    put = put . serialize
    get = deserialize <$> get
