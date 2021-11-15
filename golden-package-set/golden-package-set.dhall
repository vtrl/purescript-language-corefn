let conf = ../spago.dhall

in      conf
    //  { sources = conf.sources # [ "golden-package-set/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "aff"
              , "console"
              , "effect"
              , "node-buffer"
              , "node-fs"
              , "node-glob-basic"
              , "node-process"
              ]
        }
