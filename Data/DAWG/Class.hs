module Data.DAWG.Class
( DAWG (..)
) where

import Control.Applicative ((<$>))
import Data.List (find)

-- | Generic DAWG interface.  Only non-modifying operations.  Minimal complete
-- definition: valueIn, edges and root.  Compare with Trie typeclass.
class DAWG d where
    root    :: d a -> Int
    valueIn :: d a -> Int -> a
    edges   :: d a -> Int -> [(Char, Int)]
    edgeOn  :: d a -> Int -> Char -> Maybe Int

    -- | Default implementations.
    edgeOn d k x = snd <$> find ((x==).fst) (edges d k)
