{-# LANGUAGE MultiParamTypeClasses #-}

module NLP.Adicts.DAWG.Class
( DAWG (..)
, entry
) where

import Control.Applicative ((<$>))
import Data.List (find)
import Data.Maybe (listToMaybe)

-- | Generic DAWG interface.  Only non-modifying operations.  Minimal complete
-- definition: valueIn, edges and root.  Compare with Trie typeclass.
class Ord a => DAWG d a where
    root    :: d a b -> Int
    valueIn :: d a b -> Int -> b
    edges   :: d a b -> Int -> [(a, Int)]
    edgeOn  :: d a b -> Int -> a -> Maybe Int

    -- | Default implementations.
    edgeOn d k x = snd <$> find ((x==).fst) (edges d k)

entry :: DAWG d a => d a (Maybe b) -> [Int] -> Maybe ([a], b)
entry dag xs = do
    x <- mapM (charOn dag) (zip xs (tail xs))
    r <- maybeLast xs >>= valueIn dag 
    return (x, r)
  where
    maybeLast [] = Nothing
    maybeLast ys = Just $ last ys

charOn :: DAWG d a => d a b -> (Int, Int) -> Maybe a
charOn dag (r, x) = listToMaybe
    [c | (c, y) <- edges dag r, x == y]
