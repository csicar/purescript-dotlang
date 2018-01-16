Dot-Lang
========

define your model like this:

```purescript
Graph [
    NodeDef $ Node "a" [],
    NodeDef $ Node "b" [],
    EdgeDef (Edge "a" "b"),
    Subgraph [
    NodeDef $ Node "d" []
    ]
]
```

can be rendered using `toText` to:

```
graph {
    a [];
    b [];
    a -> b;
    subgraph {
        d []; 
    }
}
```