{ name = "dotlang"
, license = "MIT"
, repository = "https://github.com/csicar/purescript-dotlang.git"
, dependencies =
  [ "colors"
  , "console"
  , "effect"
  , "prelude"
  , "psci-support"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
