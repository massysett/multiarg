{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Multiarg.Maddash.Tests where

import Control.Applicative
import Multiarg.Types
import Multiarg.Maddash
import Test.QuickCheck
  ( Gen, Arbitrary(..), CoArbitrary(..),
    suchThat, listOf, oneof, forAll, Property,
    (==>), choose, vectorOf )
import Prelude.Generators (function1, function2, function3)
import qualified Prelude.Generators as G
import Makeopt

genInt :: Gen Int
genInt = arbitrary

genStringF1 :: Gen (String -> Int)
genStringF1 = function1 coarbitrary arbitrary

genStringF2 :: Gen (String -> String -> Int)
genStringF2 = function2 coarbitrary coarbitrary arbitrary

genStringF3 :: Gen (String -> String -> String -> Int)
genStringF3 = function3 coarbitrary coarbitrary coarbitrary arbitrary

genShortName :: Gen ShortName
genShortName = do
  c <- arbitrary
  case shortName c of
    Nothing -> genShortName
    Just n -> return n

genLongName :: Gen LongName
genLongName = do
  c1 <- arbitrary `suchThat` (\c -> c /= '-' && c /= '=')
  cs <- listOf (arbitrary `suchThat` (/= '='))
  case longName (c1 : cs) of
    Nothing -> error $ "could not generate long name: " ++ (c1:cs)
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

genWord :: Gen Word
genWord = fmap Word arbitrary

singleDash :: Word
singleDash = Word "-"

stopper :: Word
stopper = Word "--"

genNonOptWord :: Gen Word
genNonOptWord = oneof
  [ return singleDash
  , return stopper
  , do
      c1 <- arbitrary `suchThat` (/= '-')
      cs <- listOf arbitrary
      return $ Word (c1 : cs)
  ]

coaWord :: Word -> Gen b -> Gen b
coaWord (Word s) = coarbitrary s

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
    genF = function1 coaWord
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
prop_nonOptWordNotAnOptionIfStateIsReady :: Property
prop_nonOptWordNotAnOptionIfStateIsReady =
  forAll genShortArgSpecs $ \shorts ->
  forAll genLongArgSpecs $ \longs ->
  forAll genNonOptWord $ \token ->
  let (pallet, _) = processWord shorts longs Ready token
  in pallet == NotAnOption

-- | Stopper always returns NotAnOption if State is Ready
prop_stopperNotAnOptionIfStateIsReady :: Property
prop_stopperNotAnOptionIfStateIsReady =
  forAll genShortArgSpecs $ \shorts ->
  forAll genLongArgSpecs $ \longs ->
  let (pallet, _) = processWord shorts longs Ready stopper
  in pallet == NotAnOption

-- | Single dash always returns NotAnOption if State is Ready
prop_singleDashNotAnOptionIfStateIsReady :: Property
prop_singleDashNotAnOptionIfStateIsReady =
  forAll genShortArgSpecs $ \shorts ->
  forAll genLongArgSpecs $ \longs ->
  let (pallet, _) = processWord shorts longs Ready singleDash
  in pallet == NotAnOption

-- | processWord never returns NotAnOption when input is Pending
prop_processWordNeverReturnsNotAnOptionOnPending =
  forAll genShortArgSpecs $ \shorts ->
  forAll genLongArgSpecs $ \longs ->
  forAll genPending $ \state ->
  forAll genWord $ \token ->
  let (pallet, _) = processWord shorts longs state token
  in pallet /= NotAnOption

-- | NotAnOption is always returned with Ready
prop_processWordNotAnOptionWithReady =
  forAll genShortArgSpecs $ \shorts ->
  forAll genLongArgSpecs $ \longs ->
  forAll genState $ \state ->
  forAll genWord $ \token ->
  let (pallet, state') = processWord shorts longs state token
  in pallet == NotAnOption ==> isReady state'

pickOne :: [a] -> Gen a
pickOne ls
  | null ls = error "pickOne: null list"
  | otherwise = fmap (\ix -> ls !! ix) (choose (0, length ls - 1))

data OptionWithToks = OptionWithToks
  { owtOptName :: OptName
  , owtArgSpec :: ArgSpec Int
  , owtArgs :: [String]
  , owtWords :: [Word]
  , owtResultOuts :: [[Output Int]]
  , owtResultToks :: Maybe [Word]
  , owtExpected :: Int
  } deriving Show

instance Arbitrary OptionWithToks where
  arbitrary = do
    OptName on <- genOptName
    as <- genArgSpec
    (args, expected) <- case as of
      ZeroArg a -> return ([], a)
      OneArg f -> do
        s <- arbitrary
        return ([s], f s)
      TwoArg f -> do
        s1:s2:[] <- vectorOf 2 arbitrary
        return ([s1,s2], f s1 s2)
      ThreeArg f -> do
        s1:s2:s3:[] <- vectorOf 3 arbitrary
        return ([s1,s2,s3], f s1 s2 s3)
    let strings = case on of
          Left shrt -> processShortOptions [] (shrt, args)
          Right lng -> processLongOption lng args
    toks <- fmap (map Word) $ pickOne strings
    let (shrts, lngs) = case on of
          Left shrt -> ([(shrt, as)], [])
          Right lng -> ([], [(lng, as)])
        (procRslts, procEi) = processWords shrts lngs toks
        mayToks = either (const Nothing) Just procEi
    return $ OptionWithToks (OptName on) as args toks procRslts
      mayToks expected

prop_optionWithToksResultToksEmpty = (== Just []) . owtResultToks

prop_optionWithToksResultIsExpected owt
  = concat (owtResultOuts owt) == [Good . owtExpected $ owt]

