module Data.DotLang.Attr (Attribute, attributesToText, FoldToDotLang()) where

import Prelude

import Data.DotLang.Class (class DotLangValue, toValue)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.RowList (class RowToList)


type Attribute r
  = r -> r

data FoldToDotLang
  = FoldToDotLang

instance foldAttribtues ∷
  (IsSymbol sym, DotLangValue a) =>
  FoldingWithIndex FoldToDotLang (SProxy sym) (Array String) (Maybe a) (Array String) where
  foldingWithIndex _ _ acc Nothing = acc
  foldingWithIndex _ key acc (Just value) = acc <> [ reflectSymbol key <> "=" <> toValue value ]

attributesToText ::
  ∀ r rl.
  RowToList r rl =>
  HFoldlWithIndex FoldToDotLang (Array String) { | r } (Array String) =>
  { | r } ->
  (Array String)
attributesToText r = hfoldlWithIndex FoldToDotLang ([] :: Array String) r
