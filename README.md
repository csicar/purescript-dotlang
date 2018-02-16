Dot-Lang
========

documentation can be found on [pursuit](https://pursuit.purescript.org/packages/purescript-dotlang/1.1.0/docs/Data.DotLang#t:DotLang)

define your model like this:

```purescript
DiGraph [
    node "a" [ Shape Diamond, Style Filled,  FillColor red ],
    node "b" [],
    "a" ==> "b",
    "a" ==> "d",
    Subgraph [
    node "d" []
    ]
]
```

can be rendered using `toText` to:

```
digraph {
    a [shape=diamond ,style=filled ,fillcolor=\"#f44336\"];
    b [];
    a -> b;
    a -> d;
    subgraph {
        d []; 
    }
}
```
