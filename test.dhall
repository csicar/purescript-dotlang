let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs", "generated-doctests/**/*.purs" ],
  dependencies = conf.dependencies # [ "spec" ]
}