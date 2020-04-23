module Data.DotLang.Attr.Common where

import Prelude

import Color (Color)
import Data.DotLang.Attr (Attribute)
import Data.DotLang.Class (class DotLangValue)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

type Attributes r
  = ( color :: Maybe Color
    , fontcolor :: Maybe Color
    , fontsize :: Maybe Int
    , label :: Maybe LabelValue
    , style :: Maybe FillStyle
    , fillcolor :: Maybe Color
    , penWidth :: Maybe Number
    | r
    )

defaultAttributes :: { | Attributes () }
defaultAttributes =
  { color: Nothing
  , fontcolor: Nothing
  , fontsize: Nothing
  , label: Nothing
  , style: Nothing
  , fillcolor: Nothing
  , penWidth: Nothing
  }


color :: ∀ r. Color -> Attribute { | Attributes r }
color v = _ { color = Just v }


fontColor :: ∀ r. Color -> Attribute { | Attributes r }
fontColor v = _ { fontcolor = Just v }


fontSize :: ∀ r. Int -> Attribute { | Attributes r }
fontSize v = _ { fontsize = Just v }


style :: ∀ r. FillStyle -> Attribute { | Attributes r }
style v = _ { style = Just v }

fillColor :: ∀ r. Color -> Attribute { | Attributes r }
fillColor v = _ { fillcolor = Just v }


penWidth :: ∀ r. Number -> Attribute { | Attributes r }
penWidth v = _ { penWidth = Just v }


-- |
-- | ```purescript
-- | htmlLabel "<table><tr><td>Label</td></tr></table>" -- :: Attribute ..
-- | ```
-- | htmlLabel as a part of an attribute of a node.
htmlLabel :: ∀ r. String -> Attribute { | Attributes r }
htmlLabel text = _ { label = Just $ HtmlLabel text }

-- |
-- | ```purescript
-- | textLabel "..." -- :: Attribute ...
-- | ```
-- | label as a part of an attribute of a node.
label :: ∀ r. String -> Attribute { | Attributes r }
label text = _ { label = Just $ TextLabel text }

data FillStyle
  = Filled
  | Dotted
  | Invis

derive instance genericFillStyle :: Generic FillStyle _
instance eqFillStyle :: Eq FillStyle where eq = genericEq

instance showFillStyle :: Show FillStyle where
  show = genericShow

instance fillstyle :: DotLangValue FillStyle where
  toValue Filled = "filled"
  toValue Dotted = "dotted"
  toValue Invis = "invis"


data LabelValue
  = TextLabel String
  | HtmlLabel String

derive instance genericLabel :: Generic LabelValue _

instance showLabel :: Show LabelValue where
  show = genericShow

instance labelValue :: DotLangValue LabelValue where
  toValue (TextLabel t) = show t
  toValue (HtmlLabel t) = show t

