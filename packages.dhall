let mkPackage =
      https://raw.githubusercontent.com/spacchetti/spacchetti/20181209/src/mkPackage.dhall sha256:8e1c6636f8a089f972b21cde0cef4b33fa36a2e503ad4c77928aabf92d2d4ec9

let upstream =
      https://raw.githubusercontent.com/spacchetti/spacchetti/20181209/src/packages.dhall sha256:c63285af67ae74feb2f6eb67521712441928d2726ea10e2040774849ca765027

let overrides = {=}

let additions =
  { benchotron =
      mkPackage
        [ "arrays"
        , "exists"
        , "profunctor"
        , "strings"
        , "quickcheck"
        , "lcg"
        , "transformers"
        , "foldable-traversable"
        , "exceptions"
        , "node-fs"
        , "node-buffer"
        , "node-readline"
        , "datetime"
        , "now"
        ]
        "https://github.com/hdgarrood/purescript-benchotron.git"
        "v7.0.0",
    stacksafe-function =
      mkPackage
        [ "prelude"
        , "newtype"
        , "assert"
        , "quickcheck-laws"
        ]
        "https://github.com/safareli/purescript-stacksafe-function.git"
        "v2.0.0"
  }

in  upstream ⫽ overrides ⫽ additions
