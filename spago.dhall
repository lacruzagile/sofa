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
  , "avar"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "dom-filereader"
  , "dom-indexed"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "fork"
  , "form-urlencoded"
  , "formatters"
  , "halogen"
  , "halogen-select"
  , "halogen-subscriptions"
  , "http-methods"
  , "integers"
  , "js-date"
  , "js-uri"
  , "jwt"
  , "lists"
  , "maybe"
  , "media-types"
  , "newtype"
  , "nonempty"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "quickcheck"
  , "routing"
  , "safe-coerce"
  , "spec"
  , "spec-quickcheck"
  , "strings"
  , "stringutils"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "uuid"
  , "web-dom"
  , "web-events"
  , "web-file"
  , "web-html"
  , "web-storage"
  , "web-uievents"
  , "web-url"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
