module Data.DotLang.Attr where

import Prelude

import Data.DotLang.Class (class DotLang)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data FillStyle
  = Filled
  | Dotted
  | Invis

derive instance genericFillStyle :: Generic FillStyle _

instance showFillStyle :: Show FillStyle where
  show = genericShow

instance fillStyleDotLang :: DotLang FillStyle where
  toText Filled = "filled"
  toText Dotted = "dotted"
  toText Invis = "invis"

