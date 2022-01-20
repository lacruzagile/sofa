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
{ name = "sofa"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "arrays"
  , "b64"
  , "console"
  , "control"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign-object"
  , "form-urlencoded"
  , "formatters"
  , "halogen"
  , "halogen-select"
  , "http-methods"
  , "integers"
  , "js-date"
  , "js-uri"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "routing"
  , "simple-ajax"
  , "spec"
  , "spec-quickcheck"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "variant"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
