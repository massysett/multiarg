{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Multiarg.Maddash.Tests where

import Control.Applicative
import Multiarg.Maddash
import Test.QuickCheck
import Prelude.Generators
import qualified Prelude.Generators as G
import qualified Makeopt

genShortName :: Gen ShortName
genShortName = do
  c <- arbitrary
  case shortName c of
    Nothing -> genShortName
    Just n -> return n

genLongName :: Gen LongName
genLongName = do
  c1 <- arbitrary `suchThat` (/= '-')
  cs <- listOf (arbitrary `suchThat` (/= '='))
  case longName (c1 : cs) of
    Nothing -> error "could not generate long name"
    Just n -> return n

genOptName :: Gen OptName
genOptName = fmap OptName $ G.either genShortName genLongName

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

singleDash :: Token
singleDash = Token "-"

stopper :: Token
stopper = Token "--"

genNonOptToken :: Gen Token
genNonOptToken = oneof
  [ return singleDash
  , return stopper
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
  [ fmap BadOption genOptName
  , LongArgumentForZeroArgumentOption <$> genLongName <*> genOptArg
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

genPending :: Gen (State Int)
genPending = Pending <$> genOptName <*> genF
  where
    genF = function1 coaToken
      ((,) <$> listOf genOutput <*> genState)

genState :: Gen (State Int)
genState = oneof
  [ return Ready
  , genPending
  ]

genShortArgSpecs :: Gen [(ShortName, ArgSpec Int)]
genShortArgSpecs = listOf ((,) <$> genShortName <*> genArgSpec)

genLongArgSpecs :: Gen [(LongName, ArgSpec Int)]
genLongArgSpecs = listOf ((,) <$> genLongName <*> genArgSpec)

-- * Properties

-- | Non-option token always returns NotAnOption if State is Ready
prop_nonOptTokenNotAnOptionIfStateIsReady :: Property
prop_nonOptTokenNotAnOptionIfStateIsReady =
  forAll genShortArgSpecs $ \shorts ->
  forAll genLongArgSpecs $ \longs ->
  forAll genNonOptToken $ \token ->
  let (pallet, _) = processToken shorts longs Ready token
  in pallet == NotAnOption

-- | Stopper always returns NotAnOption if State is Ready
prop_stopperNotAnOptionIfStateIsReady :: Property
prop_stopperNotAnOptionIfStateIsReady =
  forAll genShortArgSpecs $ \shorts ->
  forAll genLongArgSpecs $ \longs ->
  let (pallet, _) = processToken shorts longs Ready stopper
  in pallet == NotAnOption

-- | Single dash always returns NotAnOption if State is Ready
prop_singleDashNotAnOptionIfStateIsReady :: Property
prop_singleDashNotAnOptionIfStateIsReady =
  forAll genShortArgSpecs $ \shorts ->
  forAll genLongArgSpecs $ \longs ->
  let (pallet, _) = processToken shorts longs Ready singleDash
  in pallet == NotAnOption

-- | processToken never returns NotAnOption when input is Pending
prop_processTokenNeverReturnsNotAnOptionOnPending =
  forAll genShortArgSpecs $ \shorts ->
  forAll genLongArgSpecs $ \longs ->
  forAll genPending $ \state ->
  forAll genToken $ \token ->
  let (pallet, _) = processToken shorts longs state token
  in pallet /= NotAnOption

-- | NotAnOption is always returned with Ready
prop_processTokenNotAnOptionWithReady =
  forAll genShortArgSpecs $ \shorts ->
  forAll genLongArgSpecs $ \longs ->
  forAll genState $ \state ->
  forAll genToken $ \token ->
  let (pallet, state') = processToken shorts longs state token
  in pallet == NotAnOption ==> isReady state'


