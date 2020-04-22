module Data.DotLang.Attr.Node where

import Prelude
import Color (Color, toHexString)
import Data.DotLang.Attr (Attribute, FillStyle, LabelValue)
import Data.DotLang.Class (class DotLang, class DotLangValue, toText, toValue)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.RowList (class RowToList)

type NodeAtributes r
  = ( color :: Maybe Color
    , margin :: Maybe Int
    , fontColor :: Maybe Color
    , fontSize :: Maybe Int
    , width :: Maybe Int
    , label :: Maybe LabelValue
    , shape :: Maybe ShapeType
    , style :: Maybe FillStyle
    , fillcolor :: Maybe Color
    , penWidth :: Maybe Number
    | r
    )

defaultNodeAttributes :: Record (NodeAtributes ())
defaultNodeAttributes =
  { color: Nothing
  , margin: Nothing
  , fontColor: Nothing
  , fontSize: Nothing
  , width: Nothing
  , label: Nothing
  , shape: Nothing
  , style: Nothing
  , fillcolor: Nothing
  , penWidth: Nothing
  }

instance shapeType :: DotLangValue ShapeType where
  toValue = toText

style :: ∀ r. FillStyle -> Attribute { style :: Maybe FillStyle | r }
style v = _ { style = Just v }

fillColor :: ∀ r. Color -> Attribute { fillcolor :: Maybe Color | r }
fillColor v = _ { fillcolor = Just v }

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

shape :: ∀ r. ShapeType -> Attribute { shape :: Maybe ShapeType | r }
shape v = _ { shape = Just v }

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
