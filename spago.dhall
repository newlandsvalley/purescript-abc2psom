{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc2psom"
, dependencies =
  [ "abc-parser"
  , "arrays"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "midi"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "rationals"
  , "school-of-music"
  , "soundfonts"
  , "transformers"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
