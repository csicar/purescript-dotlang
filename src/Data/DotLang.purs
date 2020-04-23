module Data.DotLang where

import Prelude
import Color (Color, toHexString)
import Data.Array (foldr, null)
import Data.DotLang.Attr (Attribute, attributesToText)
import Data.DotLang.Attr.Common (label)
import Data.DotLang.Attr.Common as Gloabl
import Data.DotLang.Attr.Edge as Edge
import Data.DotLang.Attr.Global as Global
import Data.DotLang.Attr.Node as Node
import Data.DotLang.Class (class DotLang, toText)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Prelude (class Show, ($), (<$>), (<>))

-- | type alias for a Nodes Name
type Id
  = String

-- | Dot-Node
-- | example :
-- | ```purescript
-- | Node "e" [Margin 3, Label "some label"]
-- | ```
-- | is turned into: `e [margin=3, label="some label"];`
data Node
  = Node Id { | Node.Attributes }

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
changeNodeId f (Node id attr) = Node (f id) $ (label id attr)

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show = genericShow

instance nodeDotLang :: DotLang Node where
  toText (Node id attrs) = id <> " [" <> joinWith ", " (attributesToText attrs) <> "]"

data EdgeType
  = Forward
  | Backward
  | NoDir

derive instance genericEdgeType :: Generic EdgeType _

instance showEdgeType :: Show EdgeType where
  show = genericShow

instance dotLangEdgeType :: DotLang EdgeType where
  toText Forward = "->"
  toText Backward = "<-"
  toText NoDir = "--"

-- | egde from id to id
-- | `toText $ Edge Forward "a" "b" []` == `a -> b []`
-- | EdgeType determines the direction of the arrow
data Edge
  = Edge EdgeType Id Id { | Edge.Attributes }

derive instance genericEdge :: Generic Edge _

instance showEdge :: Show Edge where
  show = genericShow

instance dotLangEdge :: DotLang Edge where
  toText (Edge e id id2 attrs) = id <> " " <> (toText e) <> " " <> id2 <> attrText
    where
    attrText = case attributesToText attrs of
      [] -> ""
      textAttributes -> " [" <> joinWith ", " textAttributes <> "]"

-- | definition in a graph
data Definition
  = Global { | Global.Attributes }
  | NodeDef Node
  | EdgeDef Edge
  | Subgraph (Array Definition)

derive instance genericDefinition :: Generic Definition _

instance showDefinition :: Show Definition where
  show a = genericShow a

-- |
-- | ```purescript
-- | global [ Global.RankDir  Global.FromLeft ] -- ∷ Definition
-- | ```
-- | global as a part of a definition
global :: Array (Attribute { | Global.Attributes }) -> Definition
global = (foldr ($) Global.defaultAttributes) >>> Global

-- |
-- | ```purescript
-- | node "a" [] -- ∷ Definition
-- | ```
-- | node as a part of a definition
node :: Id → Array (Attribute { | Node.Attributes }) → Definition
node id attrs = NodeDef $ Node id (foldr ($) Node.defaultAttributes $ attrs)

-- |
-- | ```purescript
-- | edge Forward "a" "b" [] -- ∷ Definition
-- | ```
-- | edge as a part of a definition. 
-- | `==>` and `=*>` can also be used for that purpose.
edge :: EdgeType → Id → Id → Array (Attribute { | Edge.Attributes }) → Definition
edge t id id2 attrs = EdgeDef $ Edge t id id2 (foldr ($) Edge.defaultAttributes $ attrs)

forwardEdgeWithAttrs ∷ Id → Id → Array (Attribute { | Edge.Attributes }) → Definition
forwardEdgeWithAttrs = edge Forward

forwardEdge :: Id → Id → Definition
forwardEdge l r = forwardEdgeWithAttrs l r []

backwardEdgeWithAttrs ∷ Id → Id → Array (Attribute { | Edge.Attributes }) → Definition
backwardEdgeWithAttrs = edge Backward

backwardEdge ∷ Id → Id → Definition
backwardEdge l r = backwardEdgeWithAttrs l r []

normalEdgeWithAttrs ∷ Id → Id → Array (Attribute { | Edge.Attributes }) → Definition
normalEdgeWithAttrs = edge NoDir

normalEdge ∷ Id → Id → Definition
normalEdge l r = normalEdgeWithAttrs l r []

-- |
-- | ```purescript
-- | "a" ==> "b" -- :: Definition
-- | ```
-- | Forward edge as as a definition
infix 5 forwardEdge as ==>

-- |
-- | ```purescript
-- | "a" =*> "b" $ [ Edge.FillColor red ]
-- | -- toText will be: a -> b [fillcolor="#f44336"];
-- | ```
-- | Forward edge with attributes as a definition
infix 5 forwardEdgeWithAttrs as =*>

-- |
-- | ```purescript
-- | "a" <== "b" -- :: Definition
-- | ```
-- | Backward edge as a definition
infix 5 backwardEdge as <==

-- |
-- | ```purescript
-- | "a" <*= "b" $ [ Edge.FillColor red ]
-- | ```
-- | Backward edge with attributes as a definition
infix 5 backwardEdgeWithAttrs as <*=

-- |
-- | ```purescript
-- | "a" -==- "b"
-- | ```
-- | Normal edge as definition
infix 5 normalEdge as -==-

-- |
-- | ```purescript
-- | "a" =*= "b" $ [ Edge.FillColor red ]
-- | ```
-- | Normal edge with attibutes
infix 5 normalEdgeWithAttrs as =*=

instance definitionDotlang :: DotLang Definition where
  toText (Global attrs) = joinWith "; " (attributesToText attrs) <> "; "
  toText (NodeDef n) = toText n <> "; "
  toText (EdgeDef e) = toText e <> "; "
  toText (Subgraph defs) = "subgraph { " <> (joinWith "" $ toText <$> defs) <> "}"

-- | graph can either be a graph or digraph
data Graph
  = Graph (Array Definition)
  | DiGraph (Array Definition)

derive instance genericGraph :: Generic Graph _

instance showGraph :: Show Graph where
  show = genericShow

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
