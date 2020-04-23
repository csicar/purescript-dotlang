module Data.DotLang.Attr.Global where

import Prelude
import Data.DotLang.Attr (Attribute, attributesToText)
import Data.DotLang.Class (class DotLangValue, toValue)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

type Attributes
  = ( rankDir :: Maybe RankDirValue
    , pageDir :: Maybe PageDirValue
    )

defaultAttributes :: { | Attributes }
defaultAttributes = { rankDir: Nothing, pageDir: Nothing }

rankDir :: RankDirValue -> Attribute { | Attributes }
rankDir v = _ { rankDir = Just v }

pageDir :: PageDirValue -> Attribute { | Attributes }
pageDir v = _ { pageDir = Just v }

data RankDirValue
  = FromTop
  | FromLeft
  | FromBottom
  | FromRight

derive instance genericRankDirVal :: Generic RankDirValue _

instance showRankDirValue :: Show RankDirValue where
  show = genericShow

instance rankDirValueDotLangValue :: DotLangValue RankDirValue where
  toValue FromTop = "TB"
  toValue FromLeft = "LR"
  toValue FromBottom = "BT"
  toValue FromRight = "RL"

-- | Upper-case first character is major order;
-- | lower-case second character is minor order.
data PageDirValue
  = Bl
  | Br
  | Tl
  | Tr
  | Rb
  | Rt
  | Lb
  | Lt

derive instance genericPageDirValue :: Generic PageDirValue _

instance showPageDirValue :: Show PageDirValue where
  show = genericShow

instance pageDirValueDotLangValue :: DotLangValue PageDirValue where
  toValue Bl = "BL"
  toValue Br = "BR"
  toValue Tl = "TL"
  toValue Tr = "TR"
  toValue Rb = "RB"
  toValue Rt = "RT"
  toValue Lb = "LB"
  toValue Lt = "LT"
