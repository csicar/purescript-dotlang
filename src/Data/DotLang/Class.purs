module Data.DotLang.Class where

-- | `a` is a type that has a representation in the dot language
class DotLang a where
  toText :: a -> String
