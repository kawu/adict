{-# LANGUAGE RecordWildCards #-}

module Data.Adict.Graph
( search
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.List (runListT, ListT(..))
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Data.Function (on)
import Data.Maybe (fromJust, catMaybes)
import Data.List (foldl')
import qualified Data.Set as S

import Data.Trie.Class hiding (insert)
import Data.Adict.Base hiding (path)
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
search :: Trie t => ThresMod -> t (Maybe a) -> Adict [(Entry a, Double)]
search thMod trie = do 
    costVect <- initVect
    search' thMod $ S.singleton $ Node trie [] costVect 0

search' :: Trie t => ThresMod -> PQueue t a -> Adict [(Entry a, Double)]
search' thMod q
    | S.null q  = return []
    | otherwise = do
        let (n, q') = fromJust $ S.minView q
        -- tell $ "visiting: \"" ++ reverse (path n) ++ "\""
        -- tell $ ", min cost: " ++ show (snd $ minCost $ costVect n)
        -- tell $ ", queue size: " ++ show (S.size q) ++ "\n"
        ns <- successors n
        (++) <$> matchMod thMod (trie n) (costVect n) (reverse $ path n)
             <*> search' thMod (foldl' (flip S.insert) q' ns)

successors :: Trie t => Node (t a) -> Adict [Node (t a)]
successors Node{..} = runListT $ do
    (c, t) <- list (anyChild trie)
    costVect' <- lift (nextVect (depth+1) c costVect)
    guard . not . null $ costVect'
    return $ Node t (c:path) costVect' (depth+1)
  where
    list = ListT . return
