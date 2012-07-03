module Data.Trie.DAWG
( mkDAWG
) where

import Prelude hiding (mapM)
import Data.Traversable (mapM)
import qualified Data.Map as M
import qualified Control.Monad.State as ST
-- import Data.Monoid ((<>))

import Data.Trie.MapTrie

type DAG a = ST.State (M.Map (Trie a) (Trie a))

mkNode :: Ord a => Trie a -> DAG a (Trie a)
mkNode t = do
    ST.get >>= \mp -> case t `M.lookup` mp of
        Just t' -> return t'
        Nothing -> do
            xs <- mapM mkNode (edgeMap t)
            let t' = Trie (valueIn t) xs
            ST.get >>= ST.put . M.insert t' t'
            return t'

mkDAWG :: Ord a => Trie a -> Trie a
mkDAWG t = ST.evalState (mkNode t) M.empty
