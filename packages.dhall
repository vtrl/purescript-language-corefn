let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211111/packages.dhall
        sha256:7ed6350fe897a93926d16298e37d2324aabbe5eca99810204719dc3632fb555f

let overrides = {=}

let additions =
      { node-glob-basic =
        { dependencies =
          [ "aff"
          , "console"
          , "effect"
          , "lists"
          , "maybe"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "ordered-collections"
          , "strings"
          ]
        , repo = "https://github.com/natefaubion/purescript-node-glob-basic.git"
        , version = "v1.2.2"
        }
      , language-common =
        { dependencies = [ "console", "effect", "maybe", "newtype", "prelude" ]
        , repo = "https://github.com/vtrl/purescript-language-common.git"
        , version = "0bb2a7be138ad376f209b964a862891fd012dcc6"
        }
      }

in  upstream // overrides // additions
