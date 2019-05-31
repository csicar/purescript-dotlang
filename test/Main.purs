module Test.Main where

import Prelude

import Color.Scheme.MaterialDesign (red)
import Data.DotLang (Attr(..), Definition(..), FillStyle(..), Graph(..), ShapeType(..), node, toText, (==>))
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
          node "a" [ Shape Diamond, Style Filled,  FillColor red ],
          node "b" [],
          "a" ==> "b",
          "a" ==> "d",
          Subgraph [
            node "d" []
          ]
        ]
      equal "digraph {a [shape=diamond, style=filled, fillcolor=\"#f44336\"]; b []; a -> b; a -> d; subgraph { d []; }}" (toText g)
