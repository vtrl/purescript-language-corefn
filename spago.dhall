{ name = "language-purescript-corefn"
, dependencies =
  [ "argonaut-core"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "foreign-object"
  , "integers"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
