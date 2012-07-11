module Data.DAWG.Class
( DAWG (..)
, entry
) where

import Control.Applicative ((<$>))
import Data.List (find)
import Data.Maybe (listToMaybe)

-- | Generic DAWG interface.  Only non-modifying operations.  Minimal complete
-- definition: valueIn, edges and root.  Compare with Trie typeclass.
class DAWG d where
    root    :: d a -> Int
    valueIn :: d a -> Int -> a
    edges   :: d a -> Int -> [(Char, Int)]
    edgeOn  :: d a -> Int -> Char -> Maybe Int

    -- | Default implementations.
    edgeOn d k x = snd <$> find ((x==).fst) (edges d k)

entry :: DAWG d => d (Maybe a) -> [Int] -> Maybe (String, a)
entry dag xs = do
    x <- mapM (charOn dag) (zip xs (tail xs))
    r <- maybeLast xs >>= valueIn dag 
    return (x, r)
  where
    maybeLast [] = Nothing
    maybeLast xs = Just $ last xs

charOn :: DAWG d => d a -> (Int, Int) -> Maybe Char
charOn dag (root, x) = listToMaybe
    [c | (c, y) <- edges dag root, x == y]
