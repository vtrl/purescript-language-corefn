{ name = "language-corefn"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "control"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "language-common"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "safe-coerce"
  , "strings"
  , "tuples"
  , "versions"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
