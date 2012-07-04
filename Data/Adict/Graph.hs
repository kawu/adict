{-# LANGUAGE RecordWildCards #-}

module Data.Adict.Graph
( search
) where

import Control.Applicative ((<$>), (<*>))
import Data.Function (on)
import Data.Maybe (fromJust, catMaybes)
import Data.List (foldl')
import qualified Data.Set as S

import Data.Trie.Class hiding (insert)
import Data.Adict.Base
import Data.Adict.CostVect

-- | Graph node.
data Node t = Node
    -- | Trie node.
    { trie      :: t
    -- | Path from root to the trie node (inversed).
    , path      :: String
    -- | Cost vector computed for the trie node.
    , costVect  :: CostVect
    -- | Depth of the trie node in the entire trie.
    , depth     :: Pos }

-- | Node representative used for ordering in a search graph. 
proxy :: Node t -> (Double, Pos, String)
proxy = (,,) <$> snd.minCost.costVect <*> negate.depth <*> path

instance Eq (Node t) where
    (==) = (==) `on` proxy

instance Ord (Node t) where
    compare = compare `on` proxy

type PQueue t a = S.Set (Node (t (Maybe a)))

-- | TODO: Simplify by defining Entry a as (Entry a, Double).
search :: Trie t => Cost Char -> Thres -> Word
       -> t (Maybe a) -> [(Entry a, Double)]
search cost th x trie =
    here ++ lower
  where
    costVect = initVect cost th x
    queue = S.singleton (Node trie [] costVect 0)
    here = match x trie costVect []
    lower = search' cost th x queue

search' :: Trie t => Cost Char -> Thres -> Word 
        -> PQueue t a -> [(Entry a, Double)]
search' cost th x q
    | S.null q  = []
    | otherwise = here ++ lower
  where
    (n, q') = fromJust $ S.minView q
    q'' = foldl' (flip S.insert) q' (successors cost th x n)
    here = match x (trie n) (costVect n) (reverse $ path n)
    lower = search' cost th x q''

successors :: Trie t => Cost Char -> Thres -> Word
           -> Node (t a) -> [Node (t a)]
successors cost th x Node{..} = catMaybes
    [ case compVect c costVect of
        [] -> Nothing
        xs -> Just $ Node t (c:path) xs (depth+1)
    | (c, t) <- anyChild trie ]
  where
    compVect = nextVect cost th x (depth+1)
