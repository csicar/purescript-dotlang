module Test.Main where

import Prelude

import Color.Scheme.MaterialDesign (red)
import Data.DotLang (Definition(..), Graph(..), Edge(..), EdgeType(..), node, (==>), (=*>))
import Data.DotLang.Attr (FillStyle(..))
import Data.DotLang.Attr.Edge as Edge
import Data.DotLang.Attr.Node (Attr(..), ShapeType(..))
import Data.DotLang.Attr.Node as Node
import Data.DotLang.Class (toText)
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

main ∷ Effect Unit
main = runTest do
  suite "DotLang" do
    test "basic test" do
      let
        g = DiGraph [
          node "a" [ Shape Diamond, Style Filled,  Node.FillColor red ],
          node "b" [],
          "a" ==> "b",
          "a" =*> "d" $ [ Edge.FillColor red ],
          Subgraph [
            node "d" []
          ]
        ]
      equal "digraph {a [shape=diamond, style=filled, fillcolor=\"#f44336\"]; b []; a -> b; a -> d [fillcolor=\"#f44336\"]; subgraph { d []; }}" (toText g)
    test "examples from documentation" do
      equal (toText $ Edge Forward "a" "b" []) "a -> b"
      equal (toText $ "a" =*> "b" $ [ Edge.FillColor red ]) "a -> b [fillcolor=\"#f44336\"]; "