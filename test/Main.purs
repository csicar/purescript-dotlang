module Test.Main where

import Prelude
import Color.Scheme.MaterialDesign (red)
import Data.DotLang (Definition(..), Edge(..), EdgeType(..), Graph(..), edge, global, node, (=*>), (==>))
import Data.DotLang.Attr (FillStyle(..))
import Data.DotLang.Attr.Edge (arrowHead)
import Data.DotLang.Attr.Edge as Edge
import Data.DotLang.Attr.Global (RankDirValue(..))
import Data.DotLang.Attr.Global as Global
import Data.DotLang.Attr.Node (shape, style, fillColor, ShapeType(..))
import Data.DotLang.Attr.Node as Node
import Data.DotLang.Class (toText)
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

main ∷ Effect Unit
main =
  runTest do
    suite "DotLang" do
      test "basic test" do
        let
          g =
            DiGraph
              [ global [ Global.RankDir FromLeft ]
              , node "a" [ shape Diamond, style Filled, fillColor red ]
              , node "b" []
              , "a" ==> "b"
              , "a" =*> "d" $ [ fillColor red ]
              , Subgraph
                  [ node "d" []
                  ]
              ]
        equal "digraph {rankdir=LR; a [fillcolor=\"#f44336\", shape=diamond, style=filled]; b []; a -> b; a -> d [fillcolor=\"#f44336\"]; subgraph { d []; }}" (toText g)
      test "examples from documentation" do
        equal "a -> b; " (toText $ edge Forward "a" "b" [])
        equal "a -> b [fillcolor=\"#f44336\"]; " (toText $ "a" =*> "b" $ [ fillColor red ])
      test "ArrowHead"
        $ do
            equal (toText $ "a" =*> "b" $ [ arrowHead Edge.None ]) "a -> b [arrowhead=none]; "
