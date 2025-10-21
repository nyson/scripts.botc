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
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "arrays-extra"
  , "bifunctors"
  , "concur-core"
  , "concur-react"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "maybe"
  , "parsing"
  , "partial"
  , "prelude"
  , "strings"
  , "tuples"
  , "web-dom"
  , "web-html"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
