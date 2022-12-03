{ name = "dotlang"
, license = "MIT"
, repository = "https://github.com/csicar/purescript-dotlang.git"
, dependencies =
  [ "arrays"
  , "colors"
  , "console"
  , "effect"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "spec"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
