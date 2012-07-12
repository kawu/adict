{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

module Data.Map.Combine
( combine
, RelCode (..)
, poliWith
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Data.List (foldl')
import Data.Maybe (maybeToList)

combine :: Ord a => M.Map a b -> M.Map a c -> (a -> d) -> M.Map a d
combine m n f =
    let keys = S.toList (M.keysSet m `S.union` M.keysSet n)
    in  M.fromList [(x, f x) | x <- keys]

type Form  = T.Text
type Lemma = T.Text
type PoliMorf = M.Map Form Lemma

-- | Reliability information: how did we assign a particular label to
-- a particular word form.
data RelCode
    = Exact
    | ByLemma   -- ^ Label assigned based on lemma label  
    | ByForm    -- ^ Based on labels of other forms within the same lexeme.
    deriving (Eq, Ord)

instance Show RelCode where
    show Exact   = "1"
    show ByLemma = "2"
    show ByForm  = "3"

poliWith :: Ord a => PoliMorf -> M.Map Form [a]
         -> M.Map Form (Maybe ([a], RelCode))
poliWith poli labeled =
    combine poli labeled f
  where
    f x | Just ys <- M.lookup x labeled
            = Just (ys, Exact)
        | Just ys <- M.lookup x poli >>= flip M.lookup labeled
            = Just (ys, ByLemma)
        | Just ys <- M.lookup x poli >>= flip M.lookup labeled'
            = Just (ys, ByForm)
        | otherwise = Nothing
    labeled' = fmap S.toList $ fromListWith S.union
        [ (lemma, S.fromList ys)
        | (form, lemma) <- M.assocs poli
        , ys <- maybeToList (form `M.lookup` labeled) ]

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> M.Map k a
fromListWith f xs =
    let update m (!k, !x) = M.insertWith' f k x m
    in  foldl' update M.empty xs
