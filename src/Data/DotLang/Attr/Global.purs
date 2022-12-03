module Data.DotLang.Attr.Global where

import Prelude

import Data.DotLang.Class (class DotLang, toText)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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

data LabelJustValue = L | R

derive instance Generic LabelJustValue _

instance Show LabelJustValue where
  show = genericShow

instance DotLang LabelJustValue where
  toText L = "l"
  toText R = "r"

data LabelLocValue = T | B

derive instance Generic LabelLocValue _

instance Show LabelLocValue where
  show = genericShow

instance DotLang LabelLocValue where
  toText T = "t"
  toText B = "b"

data StyleValue = Filled | Striped | Rounded

derive instance Generic StyleValue _

instance Show StyleValue where
  show = genericShow

instance DotLang StyleValue where
  toText Filled = "filled"
  toText Striped = "striped"
  toText Rounded = "rounded"

data Attr
  = RankDir RankDirValue
  | PageDir PageDirValue
  | Label String
  | LabelJust LabelJustValue
  | LabelLoc LabelLocValue
  | Compound Boolean
  | Style StyleValue

derive instance genericAttr :: Generic Attr _

instance showAttr :: Show Attr where
  show = genericShow

instance attrDotLang :: DotLang Attr where
  toText (RankDir dir) = "rankdir=" <> toText dir
  toText (PageDir dir) = "pagedir=" <> toText dir
  toText (Label val) = "label=" <> show val
  toText (LabelJust val) = "labeljust=" <> toText val
  toText (LabelLoc val) = "labelloc=" <> toText val
  toText (Compound val) = "compound=" <> show val
  toText (Style val) = "style=" <> toText val
