module Data.DotLang where

import Data.DotLang.Attr.Edge as Edge
import Data.DotLang.Attr.Node as Node
import Data.DotLang.Class (class DotLang, toText)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (joinWith)
import Prelude (class Show, ($), (<$>), (<>))

-- | type alias for a Nodes Name
type Id = String

-- | Dot-Node
-- | example :
-- | ```purescript
-- | Node "e" [Margin 3, Label "some label"]
-- | ```
-- | is turned into: `e [margin=3, label="some label"];`
data Node = Node Id (Array Node.Attr)


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
changeNodeId f (Node id attr) = Node (f id) $ attr <> [Node.Label id]

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show = genericShow

instance nodeDotLang :: DotLang Node where
  toText (Node id attrs) = id <> " [" <> joinWith ", " (toText <$> attrs) <> "]"


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
data Edge = Edge EdgeType Id Id (Array Edge.Attr)

derive instance genericEdge :: Generic Edge _

instance showEdge :: Show Edge where
  show = genericShow

instance dotLangEdge :: DotLang Edge where
  toText (Edge e id id2 attrs) = id <> " " <> (toText e) <> " " <> id2 <> " [" <> joinWith ", " (toText <$> attrs) <> "]"

-- | definition in a graph
data Definition
  = NodeDef Node
  | EdgeDef Edge
  | Subgraph (Array Definition)

node :: Id → Array Node.Attr → Definition
node id attrs = NodeDef $ Node id attrs

edge :: EdgeType → Id → Id → Array Edge.Attr → Definition
edge t id id2 attrs = EdgeDef $ Edge t id id2 attrs

forwardEdge :: Id → Id → Definition
forwardEdge l r = edge Forward l r []

backwardEdge :: Id → Id → Definition
backwardEdge l r = edge Backward l r []

normalEdge :: Id → Id → Definition
normalEdge l r = edge NoDir l r []

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

