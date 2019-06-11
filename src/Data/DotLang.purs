module Data.DotLang where

import Color (Color, toHexString)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (joinWith)
import Prelude (class Show, show, ($), (<$>), (<>))

-- | type alias for a Nodes Name
type Id = String

-- | possible node shapes
data ShapeType
  = Box | Polygon | Ellipse | Oval | Circle | Point | Egg
  | Triangle | Plaintext | Plain | Diamond | Trapezium | Parallelogram 
  | House | Pentagon | Hexagon | Septagon | Octagon | Doublecircle
  | Doubleoctagon | Tripleoctagon | Invtriangle | Invtrapezium 
  | Invhouse | Mdiamond | Msquare | Mcircle | Rect | Rectangle | Square
  | Star | None | Underline | Cylinder | Note | Tab | Folder | Box3d
  | Component | Promoter | Cds | Terminator | Utr | Primersite | Restrictionsite
  | Fivepoverhang | Threepoverhang | Noverhang | Assembly | Signature
  | Insulator | Ribosite | Rnastab | Proteasesite | Proteinstab | Rpromoter
  | Rarrow | Larrow | Lpromoter

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

data FillStyle
  = Filled
  | Dotted
  | Invis

derive instance genericFillStyle :: Generic FillStyle _

instance showFillStyle :: Show FillStyle where
  show = genericShow

instance fillStyleDotLang :: DotLang FillStyle where
  toText Filled = "filled"
  toText Dotted = "dotted"
  toText Invis = "invis"

data Attr
  = Margin Int
  | FontColor Color
  | FontSize Int
  | Width Int
  | Label String
  | Shape ShapeType
  | Style FillStyle
  | FillColor Color
  | PenWidth Number

derive instance genericAttr :: Generic Attr _

instance showAttr :: Show Attr where
  show = genericShow

instance attrDotLang :: DotLang Attr where
  toText (Margin i) = "margin="<> show i
  toText (FontColor s) = "fontcolor=\"" <> toHexString s <> "\""
  toText (FontSize i) = "fontsize="<> show i
  toText (Width i) = "width="<> show i
  toText (Shape t) = "shape="<> (toText t)
  toText (Style f) = "style="<>(toText f)
  toText (Label t) = "label="<> show t
  toText (FillColor c) = "fillcolor=\"" <> toHexString c <> "\""
  toText (PenWidth i) = "penwidth="<> show i

-- | Dot-Node
-- | example :
-- | ```purescript
-- | Node "e" [Margin 3, Label "some label"]
-- | ```
-- | is turned into: `e [margin=3, label="some label"];`
data Node = Node Id (Array Attr)


-- | get a nodes id
-- | example:
-- | ```purescript
-- | nodeId (Node "e" [Label "foo"]) == "e"
-- | ```
nodeId :: Node -> Id
nodeId (Node id _) = id

-- | change Nodes id to a new one; keeing the old id as the label
-- | example: `mapNodeId (\a -> a+"!") (Node "e" []) == Node "e!" [Label "e"]`
changeNodeId :: (Id -> Id) -> Node -> Node
changeNodeId f (Node id attr) = Node (f id) $ attr <> [Label id]

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show = genericShow

instance nodeDotLang :: DotLang Node where
  toText (Node id attrs) = id <> " [" <> (joinWith ", " (toText <$> attrs)) <> "]"


data EdgeType
  = Forward
  | Backward
  | NoDir

derive instance genericEdgeType :: Generic EdgeType _

instance showEdgeType :: Show EdgeType where show = genericShow 

instance dotLangEdgeType :: DotLang EdgeType where
  toText Forward = "->"
  toText Backward = "<-"
  toText NoDir = "--"

-- | egde from id to id
-- | `toText $ Edge "a" "b"` == `a -> b`
-- | option for different arrows is missing
data Edge = Edge EdgeType Id Id

derive instance genericEdge :: Generic Edge _

instance showEdge :: Show Edge where
  show = genericShow

instance dotLangEdge :: DotLang Edge where
  toText (Edge e id id2) = id <> " " <> (toText e) <> " " <> id2

-- | definition in a graph
data Definition
  = NodeDef Node
  | EdgeDef Edge
  | Subgraph (Array Definition)

node :: Id → Array Attr → Definition
node id attrs = NodeDef $ Node id attrs

edge :: EdgeType → Id → Id → Definition 
edge t id id2 = EdgeDef $ Edge t id id2

forwardEdge :: Id → Id → Definition
forwardEdge = edge Forward

backwardEdge :: Id → Id → Definition
backwardEdge = edge Backward

normalEdge :: Id → Id → Definition
normalEdge = edge NoDir

infix 5 forwardEdge as ==>
infix 5 backwardEdge as <==
infix 5 normalEdge as -==-

instance definitionDotlang :: DotLang Definition where
  toText (NodeDef n) = toText n <> "; "
  toText (EdgeDef e) = toText e <> "; "
  toText (Subgraph defs) = "subgraph { " <> (joinWith "" $ toText <$> defs) <> "}"

-- | graph can either be a graph or digraph
data Graph
  = Graph (Array Definition)
  | DiGraph (Array Definition)


instance graphDotLang :: DotLang Graph where
  toText (Graph defs) = "graph {" <> (joinWith "" $ toText <$> defs) <> "}"
  toText (DiGraph defs) = "digraph {" <> (joinWith "" $ toText <$> defs) <> "}"

-- | create graph from Nodes and Edges
-- | example: `graphFromElements [Node "e" [], Node "d" []] [Edge "e" "f"]`
graphFromElements :: Array (Node) -> Array (Edge) -> Graph
graphFromElements n e = DiGraph $ (NodeDef <$> n) <> (EdgeDef <$> e)

-- | `a` is a type that can be represented by a Dot-Graph
class GraphRepr a where
  toGraph :: a -> Graph

-- | `a` is a type that has a representation in the dot language
class DotLang a where
  toText :: a -> String
