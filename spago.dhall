{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "lang"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "generics-rep"
  , "globals"
  , "heterogeneous"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "pairs"
  , "psci-support"
  , "read"
  , "spec"
  , "strings"
  , "tuples"
  , "unicode"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
