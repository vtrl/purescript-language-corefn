{ name = "language-purescript-corefn"
, dependencies =
  [ "argonaut-core"
  , "console"
  , "effect"
  , "either"
  , "foreign-object"
  , "maybe"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
