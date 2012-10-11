-- | This module exports main data types and functions of the adict library.

module NLP.Adict
(
-- * Dictionary representation
-- $data-structures

-- ** Trie
  Trie (..)
, TrieD
, fromList
, implicitDAWG

-- ** Directed acyclic word graph
, DAWG (..)
, Row (..)
, DAWGD
, fromTrie
, fromDAWG
) where

import NLP.Adict.Trie (Trie (..), TrieD, fromList, implicitDAWG)
import NLP.Adict.DAWG (DAWG (..), Row (..), DAWGD, fromTrie, fromDAWG)

{- $data-structures

  The library provides two basic data structures used for dictionary
  representation. The first one is a 'Trie', which can be constructed 
  from a list of dictionary entries by using the 'fromList' function.

  The trie can be translated into a directed acyclic word graph ('DAWG')
  using the 'fromTrie' function (for the moment it is done in an
  inefficient manner, though). 

  There is also a possibility of constructing an implicit DAWG, i.e. a DAWG
  which is algebraically represented by a trie with sharing of common subtries,
  by using the 'implicitDAWG' function (which is also inefficient right now;
  in fact, the 'fromTrie' function uses this one underneath).

  Finally, the DAWG can be transformed back to a trie (implicit DAWG) using
  the 'fromDAWG' function.

-}

--   2. Approximate search and cost representation
--    * Plain cost function
--    * Cost components divided with respect to weight
-- 
--   There are to ways of representing the cost function, depending on
--   the searching algorithm you are planning to use.  If you want to
--   find all matches within the given distance of the query word,
--   use the 'findAll' function with cost function represented by the
--   'Cost' structure.
-- 
--   If, however, only the nearest match is needed you can use the
--   'findNearest' function. The shortest-path-search algorithm in the
--   background is optimized to use the more find-grained, 'CostDiv'
--   structure for cost representation. See the '...' module for the
--   details about how such a cost function can be constructed.
-- 
-- -}
