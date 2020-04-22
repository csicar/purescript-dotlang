module Data.DotLang.Attr where

import Prelude

import Data.DotLang.Class (class DotLang, class DotLangValue)
import Data.DotLang.Class (class DotLang, class DotLangValue, toValue)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.RowList (class RowToList)

data FillStyle
  = Filled
  | Dotted
  | Invis

derive instance genericFillStyle :: Generic FillStyle _
instance eqFillStyle :: Eq FillStyle where eq = genericEq

instance showFillStyle :: Show FillStyle where
  show = genericShow

instance fillStyleDotLang :: DotLang FillStyle where
  toText Filled = "filled"
  toText Dotted = "dotted"
  toText Invis = "invis"

instance fillstyle :: DotLangValue FillStyle where
  toValue Filled = "filled"
  toValue Dotted = "dotted"
  toValue Invis = "invis"


data LabelValue
  = TextLabel String
  | HtmlLabel String

derive instance genericLabel :: Generic LabelValue _

instance showLabel :: Show LabelValue where
  show = genericShow

instance labelValue :: DotLangValue LabelValue where
  toValue (TextLabel t) = show t
  toValue (HtmlLabel t) = show t

-- |
-- | ```purescript
-- | htmlLabel "<table><tr><td>Label</td></tr></table>" -- :: Attr
-- | ```
-- | htmlLabel as a part of an attribute of a node.
htmlLabel :: ∀ r. String -> Attribute { label :: Maybe LabelValue | r }
htmlLabel text = _ { label = Just $ HtmlLabel text }

-- |
-- | ```purescript
-- | textLabel "..." -- :: Attr
-- | ```
-- | label as a part of an attribute of a node.
label :: ∀ r. String -> Attribute { label :: Maybe LabelValue | r }
label text = _ { label = Just $ TextLabel text }


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
