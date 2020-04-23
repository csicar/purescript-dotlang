{ name = "dotlang"
, license = "MIT"
, repository = "https://github.com/csicar/purescript-dotlang.git"
, dependencies =
  [ "colors"
  , "console"
  , "effect"
  , "generics-rep"
  , "heterogeneous"
  , "prelude"
  , "psci-support"
  , "strings"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
