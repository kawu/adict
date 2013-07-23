-- | This module re-exports main data types and functions from the adict library.

module NLP.Adict
(
-- * Approximate searching 
-- $searching

-- ** Cost function 
  Word
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
import NLP.Adict.Brute (bruteSearch)
import NLP.Adict.Basic (findAll)
import NLP.Adict.Nearest (findNearest)

{- $searching

  There are three approximate searching methods implemented in
  the library.  The first one, 'findAll', can be used to find
  all matches within the given distance from the query word.
  The 'findNearest' function, on the other hand, searches only
  for the nearest to the query word match.  
  The third one, 'bruteSearch', is provided only for reference
  and testing purposes.

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
