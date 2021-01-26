{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc2psom"
, dependencies = [ "abc-parser", "school-of-music", "soundfonts" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
