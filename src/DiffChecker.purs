module DiffChecker where

import Prelude

import Control.Alt ((<|>))
import Data.Array (foldl, foldr, mapMaybe, nub, zip, (:))
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.Lens (Prism', is, preview)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.StrMap (StrMap, empty, fold, insert, isEmpty, keys, lookup)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import DiffChecker.Types (Diff, Json(..), array, boolean, int, number, onlyUndefined, string, strmap)

filter :: Json → Json → Maybe Diff
filter a b =
  (eval int I a b)
  <|> (eval string S a b)
  <|> (eval number N a b)
  <|> (eval boolean B a b)
  <|> (evalA <$> preview array a <*> preview array b)
  <|> (join $ evalM <$> preview strmap a <*> preview strmap b)
  <|> (ifUndefined a b)

  where
    evalA xs ys =
      bimap A A
        $ foldEvalA
        $ mapMaybe (uncurry filter)
        $ zip xs ys
    evalM xm ym =
      bimap Map Map
        <$> foldEvalM (filterMap filter xm ym)

    foldEvalA =
      foldr (\x → bimap ((:) (fst x)) ((:) (snd x))) $ Tuple [] []

    foldEvalM =
      fold insertOnBoth (Tuple empty empty)
        >>> \map →
          if on (&&) isEmpty (fst map) (snd map)
            then Nothing
            else Just map

    ifUndefined Undefined y = if is onlyUndefined y then Nothing else Just (Tuple Undefined y)
    ifUndefined x Undefined = if is onlyUndefined x then Nothing else Just (Tuple x Undefined)
    ifUndefined _ _ = Nothing

    insertOnBoth map key x = bimap (insert key $ fst x) (insert key $ snd x) map

filterMap :: (Json → Json → Maybe Diff) → StrMap Json → StrMap Json → StrMap Diff
filterMap f a b =
  foldl addOnlyJust empty
    $ nub
    $ on (<>) keys a b

  where
    addOnlyJust acc key =
      maybe acc (flip (insert key) acc)
        $ on f (fromMaybe Undefined <<< lookup key) a b

filterMapWithKey :: (String → Json → Json → Maybe Diff) → StrMap Json → StrMap Json → StrMap Diff
filterMapWithKey f a b =
  foldl addOnlyJust empty
    $ nub
    $ on (<>) keys a b

  where
    addOnlyJust acc key =
      maybe acc (flip (insert key) acc)
        $ on (f key) (fromMaybe Undefined <<< lookup key) a b


eval :: ∀ a. Eq a ⇒ Prism' Json a → (a → Json) → Json → Json → Maybe Diff
eval prism constructor a b =
  join
    $ (\x y →
        if x /= y
          then Just $ on Tuple constructor x y
          else Nothing)
    <$> preview prism a
    <*> preview prism b


-- ** Useful Combinators ** --

s :: ∀ x y z. (x → y → z) → (x → y) → x → z
s f g x = x `f` g x
