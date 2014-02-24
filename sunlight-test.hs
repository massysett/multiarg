module Main where

import Test.Sunlight

ghc v = (v, "ghc-" ++ v, "ghc-pkg-" ++ v)

inputs = TestInputs
  { tiDescription = Nothing
  , tiCabal = "cabal"
  , tiLowest = ghc "7.4.1"
  , tiDefault = [ ghc "7.4.1", ghc "7.6.3" ]
  , tiTest = []
  }

main = runTests inputs
