{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | Types for the testing of non-mode parsers.

module Telly where

import Multiarg.Internal
import Multiarg.Types
import Test.QuickCheck
import Control.Applicative
import Ernie

-- | A data type to hold the result of command line parsing.
data Telly
  = PosArg String
  -- ^ Positional argument

  | Empty
  -- ^ @--empty@, @-e@
  | Single String
  -- ^ @--single@, @-s@
  | Double String String
  -- ^ @--double@, @-d@
  | Triple String String String
  -- ^ @--triple@, @-t@

  | Zero
  -- ^ @-0@
  | One String
  -- ^ @-1@
  | Two String String
  -- ^ @-2@
  | Three String String String
  -- ^ @-3@

  | Cero
  -- ^ @--cero@
  | Uno String
  -- ^ @--uno@
  | Dos String String
  -- ^ @--dos@
  | Tres String String String
  -- ^ @--tres@
  deriving (Eq, Ord, Show)

optSpecs :: [OptSpec Telly]
optSpecs =
  [ optSpec "e" ["empty"] (ZeroArg Empty)
  , optSpec "s" ["single"] (OneArg Single)
  , optSpec "d" ["double"] (TwoArg Double)
  , optSpec "t" ["triple"] (ThreeArg Triple)

  , optSpec "0" [] (ZeroArg Zero)
  , optSpec "1" [] (OneArg One)
  , optSpec "2" [] (TwoArg Two)
  , optSpec "3" [] (ThreeArg Three)

  , optSpec "" ["cero"] (ZeroArg Cero)
  , optSpec "" ["uno"] (OneArg Uno)
  , optSpec "" ["dos"] (TwoArg Dos)
  , optSpec "" ["tres"] (ThreeArg Tres)
  ]

parse :: [String] -> ParsedCommandLine Telly
parse = parseCommandLine optSpecs PosArg

-- | Generates any option.
option :: Gen Telly
option = oneof
  [ return Empty
  , Single <$> optArg
  , Double <$> optArg <*> optArg
  , Triple <$> optArg <*> optArg <*> optArg

  , return Zero
  , One <$> optArg
  , Two <$> optArg <*> optArg
  , Three <$> optArg <*> optArg <*> optArg

  , return Cero
  , Uno <$> optArg
  , Dos <$> optArg <*> optArg
  , Tres <$> optArg <*> optArg <*> optArg
  ]

tellyToNestedList :: Telly -> [[String]]
tellyToNestedList telly = case telly of
  PosArg s -> [[s]]
  Empty -> long "empty" [] ++ short 'e' []
  Single s -> long "single" [s] ++ short 's' [s]
  Double s1 s2 -> long "double" [s1, s2] ++ short 'd' [s1, s2]
  Triple s1 s2 s3 -> long "triple" [s1, s2, s3]
    ++ short 't' [s1, s2, s3]

  Zero -> short '0' []
  One s -> short '1' [s]
  Two s1 s2 -> short '2' [s1, s2]
  Three s1 s2 s3 -> short '3' [s1, s2, s3]

  Cero -> long "cero" []
  Uno s -> long "uno" [s]
  Dos s1 s2 -> long "dos" [s1, s2]
  Tres s1 s2 s3 -> long "tres" [s1, s2, s3]

tellyToStrings :: Telly -> Gen [String]
tellyToStrings = pickItem . tellyToNestedList

validTellyStrings :: Gen ([Telly], [String])
validTellyStrings = do
  unneededStopper <- arbitrary
  (start, end) <- interspersedLine option PosArg
  let startStrings = map tellyToNestedList start
      endStrings = map tellyToNestedList end
  startList <- fmap concat $ mapM pickItem startStrings
  endList <- fmap concat $ mapM pickItem endStrings
  let endList'
        | null end && not unneededStopper = endList
        | otherwise = "--" : endList
  return (start ++ end, startList ++ endList')

prop_parseStringsYieldsTellies
  = forAll validTellyStrings $ \(tellies, strings) ->
  let ParsedCommandLine ls _
        = parseCommandLine optSpecs PosArg strings
  in map Right tellies === ls

prop_parseStringsYieldsNoEndError
  = forAll validTellyStrings $ \(_, strings) ->
  let ParsedCommandLine _ mayOpt
        = parseCommandLine optSpecs PosArg strings
  in mayOpt === Nothing
