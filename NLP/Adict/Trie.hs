-- | A (prefix) trie.

module NLP.Adict.Trie
( Trie (..)
, TrieM
, empty
, insert
, fromList
, toList
, follow
, lookup
, fromLang
, implicitDAWG
) where

import Prelude hiding (lookup)
import NLP.Adict.Trie.Internal
