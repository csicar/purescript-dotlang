module Data.DotLang.Attr.Edge where

import Prelude
import Data.DotLang.Attr (Attribute)
import Data.DotLang.Attr.Common as Common
import Data.DotLang.Class (class DotLangValue)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Record as Record

data ArrowHeadStyle
  = Normal
  | Inv
  | Dot
  | InvDot
  | ODot
  | InvODot
  | None
  | Tee
  | Empty
  | InvEmpty
  | Diamond
  | ODiamond
  | EDiamond
  | Crow
  | Box
  | OBox
  | Open
  | HalfOpen
  | Vee

derive instance genericArrowHeadStyle :: Generic ArrowHeadStyle _

instance showArrowHeadStyle :: Show ArrowHeadStyle where
  show = genericShow

instance arrowHeadStyleValue :: DotLangValue ArrowHeadStyle where
  toValue Normal = "normal"
  toValue Inv = "inv"
  toValue Dot = "dot"
  toValue InvDot = "invdot"
  toValue ODot = "odot"
  toValue InvODot = "invodot"
  toValue None = "none"
  toValue Tee = "tee"
  toValue Empty = "empty"
  toValue InvEmpty = "invempty"
  toValue Diamond = "diamond"
  toValue ODiamond = "odiamond"
  toValue EDiamond = "ediamond"
  toValue Crow = "crow"
  toValue Box = "box"
  toValue OBox = "obox"
  toValue Open = "open"
  toValue HalfOpen = "halfopen"
  toValue Vee = "vee"

arrowHead :: ∀ r. ArrowHeadStyle -> Attribute { arrowhead :: Maybe ArrowHeadStyle | r }
arrowHead v = _ { arrowhead = Just v }

type Attributes
  = Common.Attributes
      ( arrowhead :: Maybe ArrowHeadStyle
      )

defaultAttributes :: Record Attributes
defaultAttributes =
  Common.defaultAttributes
    `Record.disjointUnion`
      { arrowhead: Nothing
      }
