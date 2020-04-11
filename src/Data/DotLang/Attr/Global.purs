module Data.DotLang.Attr.Global where

import Prelude

import Data.DotLang.Class (class DotLang, toText)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data RankDirValue
  = FromTop
  | FromLeft
  | FromBottom
  | FromRight

derive instance genericRankDirVal :: Generic RankDirValue _

instance showRankDirValue :: Show RankDirValue where
  show = genericShow

instance rankDirValueDotLang :: DotLang RankDirValue where
  toText FromTop = "TB"
  toText FromLeft = "LR"
  toText FromBottom = "BT"
  toText FromRight = "RL"

-- | Upper-case first character is major order;
-- | lower-case second character is minor order.
data PageDirValue = Bl | Br | Tl | Tr | Rb | Rt | Lb | Lt

derive instance genericPageDirValue :: Generic PageDirValue _

instance showPageDirValue :: Show PageDirValue where
  show = genericShow

instance pageDirValueDotLang :: DotLang PageDirValue where
  toText Bl = "BL"
  toText Br = "BR"
  toText Tl = "TL"
  toText Tr = "TR"
  toText Rb = "RB"
  toText Rt = "RT"
  toText Lb = "LB"
  toText Lt = "LT"

data Attr
  = RankDir RankDirValue
  | PageDir PageDirValue
  | Concentrate Boolean

derive instance genericAttr :: Generic Attr _

instance showAttr :: Show Attr where
  show = genericShow

instance attrDotLang :: DotLang Attr where
  toText (RankDir dir) = "rankdir=" <> toText dir
  toText (PageDir dir) = "pagedir=" <> toText dir
  toText (Concentrate concentrate) = "concentrate=" <> show concentrate
