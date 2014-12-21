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

data GroverOpts
  = GOInts [GroverOpt Int]
  | GOStrings [GroverOpt String]
  | GOMaybes [GroverOpt (Maybe Int)]
  deriving (Eq, Ord, Show)

instance Arbitrary GroverOpts where
  arbitrary = oneof
    [ fmap GOInts . listOf . genGroverOpt $ arbitrary
    , fmap GOStrings . listOf . genGroverOpt $ arbitrary
    , fmap GOMaybes . listOf . genGroverOpt $ arbitrary
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

-- | A valid Grover AST, combined with a set of strings that, when
-- parsed, should yield that AST.
data ValidGrover
  = ValidGrover [Global] (Either [String] Result) [String]
  -- ^ @ValidGrover a b c@, where
  --
  -- @a@ is the list of global options
  --
  -- @b@ is either a list of strings (indicates that the user entered
  -- no mode), or the mode, and its associated options
  --
  -- @c@ is a list of strings that, when parsed, should return @a@ and @b@.
  deriving (Eq, Ord, Show)

instance Arbitrary ValidGrover where
  arbitrary = do
    globals <- listOf genGlobal
    glblStrings <- fmap concat . mapM pickItem
      . map globalToNestedList $ globals
    (ei, endStrings) <- oneof [ modeInt, modeString, modeMaybe, noMode ]
    return $ ValidGrover globals ei (glblStrings ++ endStrings)

modeInt :: Gen (Either [String] Result, [String])
modeInt = undefined

modeString :: Gen (Either [String] Result, [String])
modeString = undefined

modeMaybe :: Gen (Either [String] Result, [String])
modeMaybe = undefined

noMode :: Gen (Either [String] Result, [String])
noMode = undefined


prop_alwaysTrue = True
