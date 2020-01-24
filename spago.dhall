{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "foldable-traversable"
    , "maybe"
    , "ordered-collections"
    , "psci-support"
    , "strings"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
