{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "exceptions"
    , "foldable-traversable"
    , "maybe"
    , "newtype"
    , "numbers"
    , "ordered-collections"
    , "psci-support"
    , "strings"
    , "stringutils"
    , "unicode"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
