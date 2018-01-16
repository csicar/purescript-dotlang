Dot-Lang
========

define your model like this:

```purescript
Graph [
    node "a" [],
    node "b" [],
    "a" ==> "b",
    Subgraph [
    node "d" []
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