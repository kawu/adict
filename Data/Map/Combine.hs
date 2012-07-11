{-# LANGUAGE PatternGuards #-}

module Data.Map.Combine
( combine
, poliWith
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- | Proces łączenia dwóch słowników: PoliMorf (PM) oraz
-- słownik nazw (NE)
-- 1. Tworzymy "kodek" dla form podstawowych z PM.
-- 2. Tworzymy Trie form z PM. W węzłach są odnośniki do
--    identyfikatorów form podstawowych.
-- 3. Tworzymy nowe drzewo Trie w sposób następujący:
--    * Dla każdej formy z PM, jeśli a) ta forma należy
--      do NE lub b) jej forma podstawowa należy do NE,
--      to w słowniku wynikowym PM' forma ta będzie miała
--      przypisaną etykietę ze słownika NE.
--    * Na koniec bierzemy sumę PM' z NE w taki sposób,
--      że jeśli forma należy zarówno do PM' jak i do NE,
--      to brana jest forma (wraz z etykietą) z PM'. 
--
-- To samo można zrealizować przy użyciu prostszej struktury
-- danych, czyli mapy:
-- 1. Tworzymy mapę PM (forma -> forma podstawowa) na podstawie
--    słownika PoliMorf.
-- 2. Tworzymy mapę NE (forma -> etykieta) na podstawie słownika nazw.
-- 3. Dla każdej formy (klucza) x z PM, jeśli a) x należy do
--    NE, nadajemy etykietę NE!x, lub b) PM!x należy do NE,
--    nadajemy etykietę NE!PM!x. Tak powstaje mapa PM' (forma -> etykieta).
-- 4. Wynikiem jest PM' `union` NE.
--    
--
-- A jak to będzie dla słownika historycznego?
-- 1. Tworzymy mapę PM (forma -> forma podstawowa) na podstawie PoliMorfa.
-- 2. Tworzymy mapę NE (forma -> HID) na podstawie słownika historycznego,
--    gdzie HID to identyfikator leksemu ze słownika historycznego.
-- 3. Docelowo, chcemy otrzymać mapę (forma -> Maybe HID).
--
--
-- Spróbujmy teraz uogólnić w jakiś sposób oba procesy:
-- 1. Mamy dwie mapy, M (X -> Y) oraz N (X -> Z).
-- 2. Docelowa mapa powinny być zdefiniowana na każdym
--    argumencie, na którym zdefiniowana jest M lub N.
-- 3. Definiujemy funkcję łączenia
--    f :: M -> N -> X -> Q
-- 4. Funckja "f" uruchamiania jest na każdym argumencie
--    typu X map M oraz N, posiada dostęp do obu map, a na
--    koniec produkuje wartość typu Q.

combine :: Ord a => M.Map a b -> M.Map a c -> (a -> d) -> M.Map a d
combine m n f =
    let keys = S.toList (M.keysSet m `S.union` M.keysSet n)
    in  M.fromList [(x, f x) | x <- keys]

type Form  = T.Text
type Lemma = T.Text
type PoliMorf = M.Map Form Lemma

poliWith :: PoliMorf -> M.Map Form a -> M.Map Form (Maybe a)
poliWith poli labeled =
    combine poli labeled f
  where
    f x | Just y <- M.lookup x labeled = Just y
        | Just y <- M.lookup x poli >>= flip M.lookup labeled = Just y
        | otherwise = Nothing
