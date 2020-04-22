module Data.DotLang.Attr.Edge where

import Prelude
import Color (Color)
import Data.DotLang.Attr (FillStyle, Attribute, LabelValue)
import Data.DotLang.Class (class DotLangValue)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

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

type EdgeAttributes r
  = ( color :: Maybe Color
    , fontcolor :: Maybe Color
    , fontsize :: Maybe Int
    , label :: Maybe LabelValue
    , style :: Maybe FillStyle
    , fillcolor :: Maybe Color
    , penwidth :: Maybe Number
    , arrowhead :: Maybe ArrowHeadStyle
    | r
    )

defaultEdgeAttributes :: Record (EdgeAttributes ())
defaultEdgeAttributes =
  { color: Nothing
  , fontcolor: Nothing
  , fontsize: Nothing
  , label: Nothing
  , style: Nothing
  , fillcolor: Nothing
  , penwidth: Nothing
  , arrowhead: Nothing
  }
