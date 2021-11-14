{ name = "language-purescript-corefn"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "maybe"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
