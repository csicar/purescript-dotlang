module Data.DotLang.Class where

import Prelude
import Color (Color, toHexString)

-- | `a` is a type that has a representation in the dot language
class DotLang a where
  toText :: a -> String

class DotLangValue a where
  toValue :: a -> String

instance color :: DotLangValue Color where
  toValue v = "\"" <> toHexString v <> "\""

instance int :: DotLangValue Int where
  toValue = show

instance number :: DotLangValue Number where
  toValue = show