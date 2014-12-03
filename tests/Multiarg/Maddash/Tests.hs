module Multiarg.Maddash.Tests where

import Control.Applicative
import Multiarg.Maddash
import Test.QuickCheck
import Prelude.Generators
import qualified Prelude.Generators as G

genShort :: Gen Short
genShort = fmap Short arbitrary

genLong :: Gen Long
genLong = fmap Long arbitrary

genOption :: Gen Option
genOption = fmap Option $ G.either genShort genLong

genArgSpec :: Gen (ArgSpec Int)
genArgSpec = oneof
  [ fmap ZeroArg arbitrary
  , fmap OneArg (function1 coarbitrary arbitrary)
  , fmap TwoArg (function2 coarbitrary coarbitrary arbitrary)
  , fmap ThreeArg (function3 coarbitrary coarbitrary coarbitrary
                             arbitrary)
  ]

genToken :: Gen Token
genToken = fmap Token arbitrary

genNonOptToken :: Gen Token
genNonOptToken = oneof
  [ return $ Token "-"
  , return $ Token "--"
  , do
      c1 <- arbitrary `suchThat` (/= '-')
      cs <- listOf arbitrary
      return $ Token (c1 : cs)
  ]

coaToken :: Token -> Gen b -> Gen b
coaToken (Token s) = coarbitrary s

genOptArg :: Gen OptArg
genOptArg = fmap OptArg arbitrary

genOptionError :: Gen OptionError
genOptionError = oneof
  [ fmap BadOption genOption
  , LongArgumentForZeroArgumentOption <$> genLong <*> genOptArg
  ]

genOutput :: Gen (Output Int)
genOutput = oneof
  [ fmap Good arbitrary
  , fmap OptionError genOptionError
  ]

genPallet :: Gen (Pallet Int)
genPallet = oneof
  [ return NotAnOption
  , fmap Full $ listOf genOutput
  ]

genState :: Gen (State Int)
genState = oneof
  [ return Ready
  , Pending <$> genOption <*> genF
  ]
  where
    genF = function1 coaToken
      ((,) <$> listOf genOutput <*> genState)

genShortArgSpecs :: Gen [(Short, ArgSpec Int)]
genShortArgSpecs = listOf ((,) <$> genShort <*> genArgSpec)

genLongArgSpecs :: Gen [(Long, ArgSpec Int)]
genLongArgSpecs = listOf ((,) <$> genLong <*> genArgSpec)

-- * Properties

-- | Non-option token always returns NotAnOption if State is Ready
prop_nonOptTokenNotAnOptionIfStateIsReady :: Property
prop_nonOptTokenNotAnOptionIfStateIsReady =
  forAll genShortArgSpecs $ \shorts ->
  forAll genLongArgSpecs $ \longs ->
  forAll genNonOptToken $ \token ->
  let (pallet, _) = processToken shorts longs Ready token
  in pallet == NotAnOption

prop_alwaysTrue :: Bool
prop_alwaysTrue = True
