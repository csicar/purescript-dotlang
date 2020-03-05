module Data.DotLang.Attr.Edge where

import Prelude

import Color (Color, toHexString)
import Data.DotLang.Attr (FillStyle)
import Data.DotLang.Class (class DotLang, toText)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data LabelValue
  = TextLabel String
  | HtmlLabel String

derive instance genericLabel :: Generic LabelValue _

instance showLabel :: Show LabelValue where
  show = genericShow

data Attr
  = Color Color
  | FontColor Color
  | FontSize Int
  | Label LabelValue
  | Style FillStyle
  | FillColor Color
  | PenWidth Number

derive instance genericAttr :: Generic Attr _

instance showAttr :: Show Attr where
  show = genericShow

instance attrDotLang :: DotLang Attr where
  toText (Color s) = "color=\"" <> toHexString s <> "\""
  toText (FontColor s) = "fontcolor=\"" <> toHexString s <> "\""
  toText (FontSize i) = "fontsize="<> show i
  toText (Style f) = "style="<> toText f
  toText (Label (TextLabel t)) = "label=" <> show t
  toText (Label (HtmlLabel t)) = "label=" <> t
  toText (FillColor c) = "fillcolor=\"" <> toHexString c <> "\""
  toText (PenWidth i) = "penwidth="<> show i

-- |
-- | ```purescript
-- | htmlLabel "<table><tr><td>Label</td></tr></table>" -- :: Attr
-- | ```
-- | htmlLabel as a part of an attribute of an edge.
htmlLabel :: String -> Attr
htmlLabel = HtmlLabel >>> Label

-- |
-- | ```purescript
-- | label "..." -- :: Attr
-- | ```
-- | label as a part of an attribute of an edge.
label :: String -> Attr
label = TextLabel >>> Label
