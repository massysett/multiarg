{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | Types for the testing of non-mode parsers.

module Telly where

import Multiarg
import Multiarg.Types
import Test.QuickCheck
import Control.Applicative

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

parse :: [String] -> Either CommandLineError [Telly]
parse = parseCommandLine optSpecs PosArg

-- | Generates a valid list of Telly options; that is, a list that the
-- user could have entered in the command line.  This list may be
-- transformed into strings, which can then be parsed and compared
-- against this original value.
--
-- Returns a pair @(a, b)@, where @a@ is everything to the left of the
-- stopper, and @b@ (if non-empty) is everything to the right of the
-- stopper.

validTellies :: Gen ([Telly], [Telly])
validTellies = (,) <$> preStopper <*> postStopper

-- | Generates options, 'PosArg' that are a single hyphen only, and
-- 'PosArg' that do not start with a hyphen; these may appear to the
-- left of a stopper.
preStopper :: Gen [Telly]
preStopper = listOf (oneof [ option, posArgLeft ])

-- | Generates any word at all, with a healthy mix of empty lists
-- (stoppers are unusual.)
postStopper :: Gen [Telly]
postStopper = oneof [ return [], listOf posArgRight ]

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

-- | Generates non-option positional arguments that appear to the left
-- of the stopper.  Cannot be preceded by a dash; can, however, be a
-- single hyphen only.
posArgLeft :: Gen Telly
posArgLeft = PosArg <$>
  frequency [ (5, startsWithNonHyphen)
            , (1, return "-") ]

-- | Generates non-option positional arguments that appear to the
-- right of the stopper.  This can be any word at all.
posArgRight :: Gen Telly
posArgRight = PosArg <$> oneof
  [ arbitrary, startsWithOneHyphen, startsWithTwoHyphens ]

-- | Generates words that start with a single hyphen.
startsWithOneHyphen :: Gen String
startsWithOneHyphen = fmap ('-':) (listOf1 arbitrary)

-- | Generates words that start with two hyphens.
startsWithTwoHyphens :: Gen String
startsWithTwoHyphens = fmap ("--" ++) arbitrary

-- | Generates words that do not start with a hyphen.
startsWithNonHyphen :: Gen String
startsWithNonHyphen = (:) <$> (arbitrary `suchThat` (/= '-'))
  <*> arbitrary

-- | Generates words for option arguments.  Ensures that some start
-- with hyphens (these are valid option arguments.)
optArg :: Gen String
optArg = oneof [ startsWithOneHyphen, startsWithNonHyphen ]

short :: Char -> ShortName
short = maybe
  (error "Telly.hs: error: could not create short name")
  id . shortName

long :: String -> LongName
long = maybe
  (error "Telly.hs: error: could not create long name")
  id . longName

prop_alwaysTrue = True
