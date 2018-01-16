module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.DotLang (Definition(..), Edge(..), EdgeType(..), Graph(..), edge, node, toText, (==>))
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
        g = Graph [
          node "a" [],
          node "b" [],
          "a" ==> "b",
          Subgraph [
            node "d" []
          ]
        ]
      equal "graph {\na [];\n b [];\n a -> b;\n subgraph {\n d [];\n }}" (toText g)
