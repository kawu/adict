module Data.Adict.Extended
( search
, Rule (..)
, mkRule
, beg
, end
) where

import Data.List (isPrefixOf, unfoldr)
import qualified Data.Map as M
import Data.Monoid

import Data.RadixTree
import Data.Adict (Entry (..))

-- TODO: Consider standard (cost 1) insertion, deletion and substitution
-- rules independently from additional, special rules. BEWARE: this idea
-- will probably clash with ruleset manipolation methods!

-- | Substitution ([a] -> [a]) with given cost.
data Rule a = Rule
    { from  :: [a]
    , cost  :: Double
    , to    :: [a]
    , onBeg :: Bool
    , onEnd :: Bool }
    deriving (Show)

-- | Make rule with default onBeg and onEnd (i.e., == False) values.
mkRule x c y = Rule x c y False False

instance Monoid (Rule a) where
    mempty = Rule [] 0.0 [] False False
    mappend (Rule x c y b e) (Rule x' c' y' b' e') =
        Rule x'' c'' y'' b'' e''
      where
        x'' = x ++ x'
        c'' = c + c'
        y'' = y ++ y'
        b'' = case b' of
            True  -> error "right rule onBeg"
            False -> b
        e'' = case e of
            True  -> error "left rule onEnd"
            False -> e'

beg :: Rule a
beg = mempty {onBeg = True}

end :: Rule a
end = mempty {onEnd = True}

-----------------------
-- Dictionary searching
-----------------------

search :: (Show a, Ord a) => [Rule a] -> Double -> [a]
       -> Trie a b -> [(Entry a b, Double)]
search rs th p trie
     = M.toList $ M.fromListWith min
     $ doSearch ([], 0.0, True) rs th p trie

-- | Find all words in a trie with overall cost lower or equall
-- to given threshold.
doSearch :: (Show a, Eq a) => ([a], Double, Bool) -> [Rule a] -> Double
         -> [a] -> Trie a b -> [(Entry a b, Double)]

doSearch (path, k, _) _ _ [] trie =
    case valueIn trie of
        Just x  -> [(Entry path x, k)]
        Nothing -> []

doSearch (path, k, isBeg) rs th p trie =

    onId ++ onInsert ++ onDelete ++ onSubst ++ onRule

  where

    onId = case child (head p) trie of
        Just trie' ->
            doSearch (path ++ [head p], k, False) rs th (tail p) trie'
        Nothing -> []

    stdOp op = if k + 1.0 > th then [] else op

    onInsert = stdOp $ concat
        [ doSearch (path ++ [x], k + 1.0, False) rs th p trie'
        | (x, trie') <- anyChild trie ]

    onDelete = stdOp $ doSearch (path, k + 1.0, False) rs th (tail p) trie

    onSubst = stdOp $ concat
        [ doSearch (path ++ [x], k + 1.0, False) rs th (tail p) trie'
        | (x, trie') <- anyChild trie, x /= head p ]

    onRule = concat
        [ doSearch (path ++ move, k + cost r, False) rs th  -- FIXME isBeg can be True!
                   (hold ++ drop (length $ from r) p) trie' 
        | r <- filter (mathing isBeg p) rs
        , k + cost r <= th
        , (hold, move, trie') <- unfoldr next (Just (to r, [], trie)) ]

    next (Just x) = Just (x, doNext x)
    next Nothing  = Nothing
    doNext ([], _, _) = Nothing
    doNext (x:hold, move, trie) =
      case child x trie of
        Just trie' -> Just (hold, move ++ [x], trie')
        Nothing -> Nothing 

-- doSearch (path, k, isBeg) rs th p trie = concat
--     [ doSearch (path ++ move, k + cost r, False) rs th  -- FIXME isBeg can be True!
--                (hold ++ drop (length $ from r) p) trie' 
--     | r <- filter (mathing isBeg p) rs
--     , k + cost r <= th
--     , (hold, move, trie') <- unfoldr next (Just (to r, [], trie)) ]
--   where
--     next (Just x) = Just (x, doNext x)
--     next Nothing  = Nothing
--     doNext ([], _, _) = Nothing
--     doNext (x:hold, move, trie) =
--       case child x trie of
--         Just trie' -> Just (hold, move ++ [x], trie')
--         Nothing -> Nothing 
    
-- * Zamieniamy (from r) na (to r) w słowie p, czyli mamy słowo (to r ++ p')
-- * Dla każdego podziału otrzymanego słowa, od ([], to r ++ p') do
--   (to r, p') wywołujemy rekurencyjnie doSearch na prawej części podziału,
--   z kosztem zwiększonym o (cost r) oraz doklejoną lewą częścią podziału. 

-- doSearch (path, k, isBeg) rs th p trie = concat
--     [ case follow (to r) trie of
--         Just trie'  -> doSearch (path ++ to r, k + cost r, False) rs th
--                                 (drop (length $ from r) p) trie' 
--         Nothing     -> []
--     | r <- filter (mathing isBeg p) rs, k + cost r <= th ]

mathing :: Eq a => Bool -> [a] -> Rule a -> Bool
mathing isBeg y r
    | onBeg r && not isBeg = False
    | onEnd r   = from r == y
    | otherwise = from r `isPrefixOf` y 

------------------------------
-- Ruleset manipulation
------------------------------
--
-- Manipulacja zbiorem reguł, czyli:
-- 1. Dodawanie nowych reguł poprzez łączenie "nachodzących" na siebie reguł,
-- 2. Usuwanie reguł, które mogą powstać (taniej lub tym samym kosztem)
--    poprzez połączenie dwóch (lub więcej?) nachodzących na siebie reguł.
--
-- Zajmiemy się zadaniem 1, a 2 pozostawimy na przyszłość.
--
-- 1. Nowe reguły tworzymy przez "łączenie" reguł, tak więc musimy zdefiniować
-- metodę połączenia dwóch reguł. Poza tym potrzebujemy również możliwość
-- porównywania reguł, żeby usuwać duplikaty, a to oznacza dodanie dla typu
-- Rule instancji klas Eq i Ord.
--
-- Na początek, rozważmy połączenie dwóch reguł. Ważna jest kolejność
-- argumentów (zakładamy, że najpierw stosowana jest reguła zadana jako
-- pierwszy argument).
-- * Szukamy części wspólnej pomiędzy (to r1) a (from r2)
-- * 

-- join :: Rule a -> Rule a -> [Rule a]
-- join r r' =
--     [
--   where
    
-- Podejdźmy do sprawy teorio-graficznie. Każda reguła reprezentuje
-- wierzchołek, a operujemy na zbiorze reguł. Chcemy, żeby w naszym
-- zbiorze nie było reguł nadmiarowych, np. żeby nie było dwóch takich
-- samych reguł różniących się jedynie kosztem (w takiej sytuacji
-- możemy wyrzucić regułę o wyższym koszcie). Należy pod tym kątem
-- rozważyć również atrybuty onBeg i onEnd poszczególnych reguł.
--
-- Tak więc, gdy będziemy mieli funkcje join, która daje nam wszystkie
-- możliwe wyniki sekwencyjnego połączenia dwóch reguł, możemy znaleźć
-- wszystkie możliwe reguły (domknąć relację join?)! ALE: oczywiście
-- reguł może być/jest nieskończenie wiele :). Najlepiej/najłatwiej
-- ograniczyć zbiór możliwych reguł, np. w sposób następujacy:
-- * length (from r) <= k
-- * length (to r)   <= k'
-- * cost r          <= c
