module Data.DotLang where

import Data.DotLang.Attr.Edge as Edge
import Data.DotLang.Attr.Node as Node
import Data.DotLang.Attr.Global as Global
import Data.DotLang.Class (class DotLang, toText)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Array (null)
import Prelude (class Show, ($), (<$>), (<>))

-- | type alias for a Nodes Name
type Id
  = String

-- | Dot-Node
--| ```purescript run
--| > import Data.DotLang
--| > import Data.DotLang.Class (toText)
--| > import Data.DotLang.Attr.Node as Node
--| > toText $ Node "e" [Node.Margin 3, Node.label "some label"]
--| "e [margin=3, label=\"some label\"]"
--| ```
data Node
  = Node Id (Array Node.Attr)

-- | get a nodes id
--| ```purescript run
--| > nodeId (Node "e" [Node.label "foo"])
--| "e"
--| ```
nodeId :: Node -> Id
nodeId (Node id _) = id

-- | change Nodes id to a new one; keeing the old id as the label
--| ```purescript run
--| > changeNodeId (_ <> "!") (Node "e" [])
--| (Node "e!" [(Label (TextLabel "e"))])
--| ```
changeNodeId :: (Id -> Id) -> Node -> Node
changeNodeId f (Node id attr) = Node (f id) $ attr <> [ Node.label id ]

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

instance showEdgeType :: Show EdgeType where
  show = genericShow

instance dotLangEdgeType :: DotLang EdgeType where
  toText Forward = "->"
  toText Backward = "<-"
  toText NoDir = "--"

-- | egde from id to id
--| ```purescript run
--| > toText $ Edge Forward "a" "b" []
--| "a -> b"
--| ```
-- | EdgeType determines the direction of the arrow
data Edge
  = Edge EdgeType Id Id (Array Edge.Attr)

derive instance genericEdge :: Generic Edge _

instance showEdge :: Show Edge where
  show = genericShow

instance dotLangEdge :: DotLang Edge where
  toText (Edge e id id2 attrs) = id <> " " <> (toText e) <> " " <> id2 <> attrText
    where
    attrText = if null attrs then "" else " [" <> joinWith ", " (toText <$> attrs) <> "]"

-- | definition in a graph
data Definition
  = Global (Array Global.Attr)
  | NodeDef Node
  | EdgeDef Edge
  | Subgraph (Array Definition)

-- |
--| ```purescript run
--| > import Data.DotLang.Attr.Global as Global
--| > :t global [Global.RankDir Global.FromLeft]
--| Definition
--| ```
-- |
-- | global as a part of a definition
global :: Array Global.Attr -> Definition
global = Global

-- |
--| ```purescript run
--| > :t node "a" []
--| Definition
--| ```
-- | node as a part of a definition
node :: Id → Array Node.Attr → Definition
node id attrs = NodeDef $ Node id attrs

-- |
--| ```purescript run
--| > :t edge Forward "a" "b" [] 
--| Definition
--| ```
-- | edge as a part of a definition. 
-- | `==>` and `=*>` can also be used for that purpose:
-- |
edge :: EdgeType → Id → Id → Array Edge.Attr → Definition
edge t id id2 attrs = EdgeDef $ Edge t id id2 attrs

forwardEdgeWithAttrs ∷ Id → Id → Array Edge.Attr → Definition
forwardEdgeWithAttrs = edge Forward

forwardEdge :: Id → Id → Definition
forwardEdge l r = forwardEdgeWithAttrs l r []

backwardEdgeWithAttrs ∷ Id → Id → Array Edge.Attr → Definition
backwardEdgeWithAttrs = edge Backward

backwardEdge ∷ Id → Id → Definition
backwardEdge l r = backwardEdgeWithAttrs l r []

normalEdgeWithAttrs ∷ Id → Id → Array Edge.Attr → Definition
normalEdgeWithAttrs = edge NoDir

normalEdge ∷ Id → Id → Definition
normalEdge l r = normalEdgeWithAttrs l r []

-- |
--| ```purescript run
--| > :t "a" ==> "b" 
--| Definition
--| ```
-- | Forward edge as as a definition
infix 5 forwardEdge as ==>

-- |
--| ```purescript run
--| > import Data.DotLang.Attr.Edge as Edge
--| > import Color.Scheme.HTML (red)
--| > toText $ "a" =*> "b" $ [ Edge.FillColor red ]
--| "a -> b [fillcolor=\"#ff0000\"]; "
--| ```
-- | Forward edge with attributes as a definition
infix 5 forwardEdgeWithAttrs as =*>

-- |
--| ```purescript run
--| > :t "a" <== "b"
--| Definition
--| ```
-- | Backward edge as a definition
infix 5 backwardEdge as <==

-- |
--| ```purescript run
--| > :t "a" <*= "b" $ [ Edge.FillColor red ]
--| Definition
--| ```
-- | Backward edge with attributes as a definition
infix 5 backwardEdgeWithAttrs as <*=

-- |
--| ```purescript run
--| > toText $ "a" -==- "b"
--| "a -- b; "
--| ```
-- | Normal edge as definition
infix 5 normalEdge as -==-

-- |
--| ```purescript run
--| > toText $ "a" =*= "b" $ [ Edge.FillColor red ]
--| "a -- b [fillcolor=\"#ff0000\"]; "
--| ```
-- | Normal edge with attibutes
infix 5 normalEdgeWithAttrs as =*=

instance definitionDotlang :: DotLang Definition where
  toText (Global attrs) = joinWith "; " (toText <$> attrs) <> "; "
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
--| ```purescript run
--| > :t graphFromElements [Node "e" [], Node "d" []] [ Edge Forward "e" "f" []]
--| Graph
--| ```
-- |
graphFromElements :: Array (Node) -> Array (Edge) -> Graph
graphFromElements n e = DiGraph $ (NodeDef <$> n) <> (EdgeDef <$> e)

-- | `a` is a type that can be represented by a Dot-Graph
class GraphRepr a where
  toGraph :: a -> Graph
