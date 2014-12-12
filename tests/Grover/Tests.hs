{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Grover.Tests where

import Ernie
import Grover
import Control.Applicative
import Test.QuickCheck hiding (Result)

genGlobal :: Gen Global
genGlobal = oneof
  [ return Help
  , Verbose <$> arbitrary
  , return Version
  ]

-- | Generates a mode option.  Does not generate positional arguments.
genGroverOpt
  :: Gen a
  -- ^ Generates arguments
  -> Gen (GroverOpt a)
genGroverOpt g = oneof
  [ return Zero
  , Single <$> g
  , Double <$> g <*> g
  , Triple <$> g <*> g <*> g
  ]

-- | Generates a Result, which incorporates the result from the
-- parsing of a mode.
genResult :: Gen Result
genResult = oneof
  [ Ints <$> listOf (fmap Right $ genGroverOpt arbitrary)
  , Strings <$> listOf (fmap Right $ genGroverOpt arbitrary)
  , Maybes <$> listOf (fmap Right $ genGroverOpt arbitrary)
  ]

globalToNestedList :: Global -> [[String]]
globalToNestedList glbl = case glbl of
  Help -> long "help" [] ++ short 'h' []
  Verbose i -> long "verbose" [show i] ++ short 'v' [show i]
  Version -> long "version" []

groverOptToNestedList :: Show a => GroverOpt a -> [[String]]
groverOptToNestedList gvr = case gvr of
  Zero -> long "zero" [] ++ short 'z' []
  Single a -> long "single" ls ++ short 's' ls
    where
      ls = [show a]
  Double a b -> long "double" ls ++ short 'd' ls
    where
      ls = [show a, show b]
  Triple a b c -> long "triple" ls ++ short 't' ls
    where
      ls = [show a, show b, show c]
  PosArg s -> [[s]]


prop_alwaysTrue = True
