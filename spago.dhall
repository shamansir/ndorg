{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "ndorg"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "halogen"
  , "halogen-subscriptions"
  , "integers"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "org"
  , "prelude"
  , "strings"
  , "text-formatting"
  , "transformers"
  , "tuples"
  , "unicode"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
