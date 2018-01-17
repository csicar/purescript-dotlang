module Test.Main where

import Prelude

import Color.Scheme.MaterialDesign (red)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.DotLang (Attr(..), Definition(..), FillStyle(..), Graph(..), ShapeType(..), node, toText, (==>))
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main ∷ Eff
        ( console :: CONSOLE
        , testOutput :: TESTOUTPUT
        , avar :: AVAR
        )
        Unit
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
