module Test.Main where

import Prelude
import Color.Scheme.MaterialDesign (red)
import Data.DotLang (Definition(..), Graph(..), Edge(..), EdgeType(..), global, node, (==>), (=*>))
import Data.DotLang.Attr (FillStyle(..))
import Data.DotLang.Attr.Edge as Edge
import Data.DotLang.Attr.Global (RankDirValue(..))
import Data.DotLang.Attr.Global as Global
import Data.DotLang.Attr.Node (Attr(..), ShapeType(..))
import Data.DotLang.Attr.Node as Node
import Data.DotLang.Class (toText)
import Effect (Effect)
import Effect.Aff (launchAff_)
import DocTest as DocTest
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run, runSpec)

main ∷ Effect Unit
main =
  launchAff_
    $ do
        runSpec [ consoleReporter ] do
          DocTest.main
          describe "DotLang" do
            it "basic test" do
              let
                g =
                  DiGraph
                    [ global [ Global.RankDir FromLeft ]
                    , node "a" [ Shape Diamond, Style Filled, Node.FillColor red ]
                    , node "b" []
                    , "a" ==> "b"
                    , "a" =*> "d" $ [ Edge.FillColor red ]
                    , Subgraph
                        [ node "d" []
                        ]
                    ]
              toText g `shouldEqual` "digraph {rankdir=LR; a [shape=diamond, style=filled, fillcolor=\"#f44336\"]; b []; a -> b; a -> d [fillcolor=\"#f44336\"]; subgraph { d []; }}"
            it "examples from documentation" do
              toText (Edge Forward "a" "b" []) `shouldEqual` "a -> b"
              toText ("a" =*> "b" $ [ Edge.FillColor red ]) `shouldEqual` "a -> b [fillcolor=\"#f44336\"]; "
            it "ArrowHead"
              $ do
                  (toText $ "a" =*> "b" $ [ Edge.ArrowHead Edge.None ]) `shouldEqual` "a -> b [arrowhead=none]; "
