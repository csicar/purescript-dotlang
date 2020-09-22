Dot-Lang
========

documentation can be found on [pursuit](https://pursuit.purescript.org/packages/purescript-dotlang/1.1.0/docs/Data.DotLang#t:DotLang)

define your model like this:

```purescript run
> import Data.DotLang
> import Data.DotLang.Attr (FillStyle(..))
> import Data.DotLang.Attr.Node as Node
> import Data.DotLang.Attr.Edge as Edge
> import Color.Scheme.HTML (red)
> exampleGraph = DiGraph [
      node "a" [ Node.Shape Node.Diamond, Node.Style Filled,  Node.FillColor red ],
      node "b" [],
      "a" ==> "b",
      "a" =*> "d" $ [ Edge.FillColor red ],
      Subgraph [
          node "d" []
      ]
  ]
> -- can be turned into a dotlang using `toText`
> import Data.DotLang.Class (toText)
> toText exampleGraph
"digraph {a [shape=diamond, style=filled, fillcolor=\"#ff0000\"]; b []; a -> b; a -> d [fillcolor=\"#ff0000\"]; subgraph { d []; }}"
```

### Installation

#### Spago

```dhall
let additions = 
    { dotlang = 
        { dependencies = 
            [ "colors"
            , "console"
            , "effect"
            , "generics-rep"
            , "prelude"
            , "psci-support"
            , "strings"
            , "test-unit"
            ]
        , repo = "https://github.com/csicar/purescript-dotlang.git"
        , version = "v2.0.0"
        }
    }
```
```bash
spago install dotlang
```

#### Bower

```bash
bower i purescript-dotlang
```



Changelog
=========

v3.0.0
------

Breaking Changes:

- `Label` of `Edge` and `Node` now support HTML and no formatting: To **migrate** replace old calls to `Label` with calls to `label`