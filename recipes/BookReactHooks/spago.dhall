{ name = "BookReactHooks"
, dependencies =
  [ "affjax"
  , "affjax-web"
  , "effect"
  , "either"
  , "exceptions"
  , "maybe"
  , "prelude"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "web-html"
  , "web-dom"
  ]
, packages = ../../packages.dhall
, sources = [ "recipes/BookReactHooks/src/**/*.purs" ]
}
