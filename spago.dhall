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
{ name = "purs-smart-spec"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "arrays"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "halogen"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
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
  , "tuples"
  , "variant"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
