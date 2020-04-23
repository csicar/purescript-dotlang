module Data.DotLang.Attr.Node (module Export, shape, ShapeType(..), margin, width, Attributes, defaultAttributes) where

import Prelude (class Show)
import Data.DotLang.Attr (Attribute)
import Data.DotLang.Attr.Common (FillStyle(..), LabelValue(..), color, fillColor, fontColor, fontSize, htmlLabel, label, penWidth, style) as Export
import Data.DotLang.Attr.Common as Common
import Data.DotLang.Class (class DotLangValue)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Record as Record

type Attributes
  = Common.Attributes
      ( margin :: Maybe Int
      , width :: Maybe Int
      , shape :: Maybe ShapeType
      )

defaultAttributes :: { | Attributes }
defaultAttributes =
  Common.defaultAttributes
    `Record.disjointUnion`
      { margin: Nothing
      , width: Nothing
      , shape: Nothing
      }

shape :: ShapeType -> Attribute { | Attributes }
shape v = _ { shape = Just v }

margin :: Int -> Attribute { | Attributes }
margin v = _ { margin = Just v }

width :: Int -> Attribute { | Attributes }
width v = _ { width = Just v }

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

derive instance genericShapeType :: Generic ShapeType _

instance showShapeType :: Show ShapeType where
  show = genericShow

instance dotLangShape :: DotLangValue ShapeType where
  toValue Box = "box"
  toValue Polygon = "polygon"
  toValue Ellipse = "ellipse"
  toValue Oval = "oval"
  toValue Circle = "circle"
  toValue Point = "point"
  toValue Egg = "egg"
  toValue Triangle = "triangle"
  toValue Plaintext = "plaintext"
  toValue Plain = "plain"
  toValue Diamond = "diamond"
  toValue Trapezium = "trapezium"
  toValue Parallelogram = "parallelogram"
  toValue House = "house"
  toValue Pentagon = "pentagon"
  toValue Hexagon = "hexagon"
  toValue Septagon = "septagon"
  toValue Octagon = "octagon"
  toValue Doublecircle = "doublecircle"
  toValue Doubleoctagon = "doubleoctagon"
  toValue Tripleoctagon = "tripleoctagon"
  toValue Invtriangle = "invtriangle"
  toValue Invtrapezium = "invtrapezium"
  toValue Invhouse = "invhouse"
  toValue Mdiamond = "mdiamond"
  toValue Msquare = "msquare"
  toValue Mcircle = "mcircle"
  toValue Rect = "rect"
  toValue Rectangle = "rectangle"
  toValue Square = "square"
  toValue Star = "star"
  toValue None = "none"
  toValue Underline = "underline"
  toValue Cylinder = "cylinder"
  toValue Note = "note"
  toValue Tab = "tab"
  toValue Folder = "folder"
  toValue Box3d = "box3d"
  toValue Component = "component"
  toValue Promoter = "promoter"
  toValue Cds = "cds"
  toValue Terminator = "terminator"
  toValue Utr = "utr"
  toValue Primersite = "primersite"
  toValue Restrictionsite = "restrictionsite"
  toValue Fivepoverhang = "fivepoverhang"
  toValue Threepoverhang = "threepoverhang"
  toValue Noverhang = "noverhang"
  toValue Assembly = "assembly"
  toValue Signature = "signature"
  toValue Insulator = "insulator"
  toValue Ribosite = "ribosite"
  toValue Rnastab = "rnastab"
  toValue Proteasesite = "proteasesite"
  toValue Proteinstab = "proteinstab"
  toValue Rpromoter = "rpromoter"
  toValue Rarrow = "Rarrow"
  toValue Larrow = "Larrow"
  toValue Lpromoter = "Lpromoter"
