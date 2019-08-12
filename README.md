Dot-Lang
========

documentation can be found on [pursuit](https://pursuit.purescript.org/packages/purescript-dotlang/1.1.0/docs/Data.DotLang#t:DotLang)

define your model like this:

```purescript
DiGraph [
    node "a" [ Shape Diamond, Style Filled,  Node.FillColor red ],
    node "b" [],
    "a" ==> "b",
    "a" =*> "d" $ [ Edge.FillColor red ],
    Subgraph [
        node "d" []
    ]
]
```

can be rendered using `toText` to:

```
digraph {
    a [shape=diamond, style=filled, fillcolor="#f44336"];
    b [];
    a -> b;
    a -> d [fillcolor="#f44336"];
    subgraph {
        d []; 
    }
}
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
