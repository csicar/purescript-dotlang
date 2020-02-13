module Data.DotLang.Attr.Edge where

import Prelude

import Color (Color, toHexString)
import Data.DotLang.Attr (FillStyle)
import Data.DotLang.Class (class DotLang, toText)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe(Maybe(..))
import Data.String.CodeUnits(charAt)

data Attr
  = Color Color
  | FontColor Color
  | FontSize Int
  | Label String
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
  toText (Label t) = case charAt 0 t of
                          Just '<' -> "label=" <> t
                          _ -> "label=" <> show t
  toText (FillColor c) = "fillcolor=\"" <> toHexString c <> "\""
  toText (PenWidth i) = "penwidth="<> show i
