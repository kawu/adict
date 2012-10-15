-- | This module re-exports main data types and functions from the adict library.

module NLP.Adict
(
-- * Dictionary representation
-- $data-structures

-- ** Trie
  Trie (..)
, TrieM
, fromList
, implicitDAWG

-- ** Directed acyclic word graph
, DAWG (..)
, Node (..)
, DAWGM
, fromTrie
, fromDAWG

-- * Approximate searching 
-- $searching

-- ** Cost function 
, Word
, Pos
, Weight
, Cost (..)
, costDefault

-- ** Searching methods
, bruteSearch
, findAll
, findNearest
) where

import NLP.Adict.Core (Word, Pos, Weight, costDefault, Cost (..))
import NLP.Adict.Trie (Trie (..), TrieM, fromList, implicitDAWG)
import NLP.Adict.DAWG (DAWG (..), Node (..), DAWGM, fromTrie, fromDAWG)
import NLP.Adict.Brute (bruteSearch)
import NLP.Adict.Basic (findAll)
import NLP.Adict.Nearest (findNearest)

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

{- $searching

  There are three approximate searching methods implemented in
  the library.  The first one, 'findAll', can be used to find
  all matches within the given distance from the query word.
  The 'findNearest' function, on the other hand, searches only
  for the nearest to the query word match.  
  The third one, 'bruteSearch', is provided only for reference
  and testing purposes.

  The 'findAll' function is evaluated against the 'Trie' while the
  'findNearest' one is evaluated against the 'DAWG'.
  The reason to make this distinction is that the 'findNearest'
  function needs to distinguish between DAG nodes and to know
  when the particular node is visited for the second time.

  Both methods perform the search with respect to the cost function
  specified by the library user, which can be used to customize
  weights of edit operations.  The 'Cost' structure provides the
  general representation of the cost and it can be used with
  the 'findAll' method.   The shortest-path algorithm used in
  the background of the 'findNearest' function is optimized to 
  use the more informative, 'CostDiv' cost representation,
  which divides edit operations between separate classes with
  respect to their weight.
-}
