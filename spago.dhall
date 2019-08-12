{ name =
    "dotlang"
, dependencies =
    [ "colors"
    , "console"
    , "effect"
    , "generics-rep"
    , "prelude"
    , "psci-support"
    , "strings"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
