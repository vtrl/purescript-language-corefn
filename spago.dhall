{ name = "language-purescript-corefn"
, dependencies =
  [ "argonaut-core"
  , "bifunctors"
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
  , "versions"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
