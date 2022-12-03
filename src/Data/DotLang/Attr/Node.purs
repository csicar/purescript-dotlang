module Data.DotLang.Attr.Node where

import Prelude

import Color (Color, toHexString)
import Data.DotLang.Class (class DotLang, toText)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)

data Style
  = Dashed
  | Dotted
  | Solid
  | Invis
  | Bold
  | Filled
  | Striped
  | Wedged
  | Diagonals
  | Rounded

derive instance Generic Style _

instance Show Style where
  show = genericShow

instance DotLang Style where
  toText Dashed = "dashed"
  toText Dotted = "dotted"
  toText Solid = "solid"
  toText Invis = "invis"
  toText Bold = "bold"
  toText Filled = "filled"
  toText Striped = "striped"
  toText Wedged = "wedged"
  toText Diagonals = "diagonals"
  toText Rounded = "rounded"

data LabelValue
  = TextLabel String
  | HtmlLabel String
  | RecordLabel RecordLabelValue

derive instance genericLabel :: Generic LabelValue _

instance showLabel :: Show LabelValue where
  show l = genericShow l

renderLabel :: LabelValue -> String
renderLabel = case _ of
  TextLabel t -> show t
  HtmlLabel t -> t
  RecordLabel recordValue -> show $ renderRecordLabelValue recordValue


data RecordLabelValue 
  = SubRecord (Array {fieldId:: Maybe String, value:: RecordLabelValue})
  | Base String

derive instance genericRecordLabelValue :: Generic RecordLabelValue _

instance showRecordLabelValue :: Show RecordLabelValue where
  show l = genericShow l

renderRecordLabelValue :: RecordLabelValue -> String
renderRecordLabelValue = case _ of
  SubRecord parts -> joinWith " | " $ map renderPart parts
    where 
      renderPart {fieldId, value} = brace "{" "}" $ (maybe "" (brace "<" ">") fieldId) <> renderRecordLabelValue value
      brace left right str =  left <> str <> right
  Base str -> str

data Attr
  = Color Color
  | Margin Int
  | FontColor Color
  | FontSize Int
  | Width Int
  | Label LabelValue
  | Shape ShapeType
  | Style Style
  | FillColor Color
  | PenWidth Number

derive instance genericAttr :: Generic Attr _

instance showAttr :: Show Attr where
  show = genericShow

instance attrDotLang :: DotLang Attr where
  toText (Margin i) = "margin=" <> show i
  toText (Color s) = "color=\"" <> toHexString s <> "\""
  toText (FontColor s) = "fontcolor=\"" <> toHexString s <> "\""
  toText (FontSize i) = "fontsize=" <> show i
  toText (Width i) = "width=" <> show i
  toText (Shape t) = "shape=" <> toText t
  toText (Style f) = "style=" <> toText f
  toText (Label l) = "label=" <> renderLabel l
  toText (FillColor c) = "fillcolor=\"" <> toHexString c <> "\""
  toText (PenWidth i) = "penwidth=" <> show i

-- | possible node shapes
data ShapeType
  = Box
  | Polygon
  | Ellipse
  | Oval
  | Circle
  | Point
  | Egg
  | Triangle
  | Plaintext
  | Plain
  | Diamond
  | Trapezium
  | Parallelogram
  | House
  | Pentagon
  | Hexagon
  | Septagon
  | Octagon
  | Doublecircle
  | Doubleoctagon
  | Tripleoctagon
  | Invtriangle
  | Invtrapezium
  | Invhouse
  | Mdiamond
  | Msquare
  | Mcircle
  | Rect
  | Rectangle
  | Square
  | Star
  | None
  | Underline
  | Cylinder
  | Note
  | Tab
  | Folder
  | Box3d
  | Component
  | Promoter
  | Cds
  | Terminator
  | Utr
  | Primersite
  | Restrictionsite
  | Fivepoverhang
  | Threepoverhang
  | Noverhang
  | Assembly
  | Signature
  | Insulator
  | Ribosite
  | Rnastab
  | Proteasesite
  | Proteinstab
  | Rpromoter
  | Rarrow
  | Larrow
  | Lpromoter
  | Record

derive instance genericShapeType :: Generic ShapeType _

instance showShapeType :: Show ShapeType where
  show = genericShow

instance dotLangShape :: DotLang ShapeType where
  toText Box = "box"
  toText Polygon = "polygon"
  toText Ellipse = "ellipse"
  toText Oval = "oval"
  toText Circle = "circle"
  toText Point = "point"
  toText Egg = "egg"
  toText Triangle = "triangle"
  toText Plaintext = "plaintext"
  toText Plain = "plain"
  toText Diamond = "diamond"
  toText Trapezium = "trapezium"
  toText Parallelogram = "parallelogram"
  toText House = "house"
  toText Pentagon = "pentagon"
  toText Hexagon = "hexagon"
  toText Septagon = "septagon"
  toText Octagon = "octagon"
  toText Doublecircle = "doublecircle"
  toText Doubleoctagon = "doubleoctagon"
  toText Tripleoctagon = "tripleoctagon"
  toText Invtriangle = "invtriangle"
  toText Invtrapezium = "invtrapezium"
  toText Invhouse = "invhouse"
  toText Mdiamond = "mdiamond"
  toText Msquare = "msquare"
  toText Mcircle = "mcircle"
  toText Rect = "rect"
  toText Rectangle = "rectangle"
  toText Square = "square"
  toText Star = "star"
  toText None = "none"
  toText Underline = "underline"
  toText Cylinder = "cylinder"
  toText Note = "note"
  toText Tab = "tab"
  toText Folder = "folder"
  toText Box3d = "box3d"
  toText Component = "component"
  toText Promoter = "promoter"
  toText Cds = "cds"
  toText Terminator = "terminator"
  toText Utr = "utr"
  toText Primersite = "primersite"
  toText Restrictionsite = "restrictionsite"
  toText Fivepoverhang = "fivepoverhang"
  toText Threepoverhang = "threepoverhang"
  toText Noverhang = "noverhang"
  toText Assembly = "assembly"
  toText Signature = "signature"
  toText Insulator = "insulator"
  toText Ribosite = "ribosite"
  toText Rnastab = "rnastab"
  toText Proteasesite = "proteasesite"
  toText Proteinstab = "proteinstab"
  toText Rpromoter = "rpromoter"
  toText Rarrow = "Rarrow"
  toText Larrow = "Larrow"
  toText Lpromoter = "Lpromoter"
  toText Record = "record"

-- |
--| ```purescript run
--| > import Data.DotLang.Attr.Node
--| > :t htmlLabel "<table><tr><td>Label</td></tr></table>"
--| Attr
--| ```
-- | htmlLabel as a part of an attribute of a node.
htmlLabel :: String -> Attr
htmlLabel = HtmlLabel >>> Label

-- |
--| ```purescript run
--| > import Data.DotLang.Attr.Node
--| > :t label "..." 
--| Attr
--| ```
-- | label as a part of an attribute of a node.
label :: String -> Attr
label = TextLabel >>> Label

--| ```purescript run
--| > import Data.DotLang
--| > import Data.DotLang.Class (toText)
--| > toText $ node "a" [recordLabel [subId "test" $ subLabel "c", subLabel "d", subRecord [ subLabel "k", subLabel "l"]]]
--| "a [label=\"{<test>c} | {d} | {{k} | {l}}\"]; "
--| ```
-- |
recordLabel :: Array {fieldId:: Maybe String, value:: RecordLabelValue} -> Attr
recordLabel = SubRecord >>> RecordLabel >>> Label

subRecord :: Array {fieldId:: Maybe String, value:: RecordLabelValue} -> {fieldId:: Maybe String, value:: RecordLabelValue} 
subRecord v = {fieldId: Nothing, value: SubRecord v }

subLabel :: String -> {fieldId :: Maybe String, value :: RecordLabelValue}
subLabel value = {fieldId : Nothing, value: Base value }

subId :: String -> {fieldId :: Maybe String, value :: RecordLabelValue} ->{fieldId :: Maybe String, value :: RecordLabelValue} 
subId str {value} = {fieldId: Just str, value: value}