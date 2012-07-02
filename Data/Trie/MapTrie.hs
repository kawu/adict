{-# LANGUAGE RecordWildCards #-}

module Data.Trie.MapTrie
( Trie (..)
) where

import qualified Data.Map as M

import qualified Data.Trie.Generic as G

data Trie a = Trie
    { valueIn :: Maybe a
    , edgeMap :: M.Map Char (Trie a) }

instance G.Trie Trie where
    empty = Trie Nothing (M.fromList [])
    anyChild Trie{..} = M.assocs edgeMap
    child x Trie{..} = x `M.lookup` edgeMap

    valueIn = valueIn
    setValue x t = t { valueIn = x }
    
    subChild x trie newChild =
        let how _ = Just newChild
        in trie { edgeMap = M.alter how x (edgeMap trie) }

instance Show a => Show (Trie a) where
    show = show . G.toList
