module System.Console.MultiArg.Error where

import System.Console.MultiArg.Option
  ( LongOpt, ShortOpt, unLongOpt, unShortOpt )
import System.Console.MultiArg.TextNonEmpty
  ( TextNonEmpty ( TextNonEmpty ) )
import Data.Text ( Text, pack, append, singleton, intercalate,
                   snoc )
import Data.Set ( Set )
import qualified Data.Set as Set
import Test.QuickCheck ( Arbitrary ( arbitrary ), 
                         choose )
import System.Console.MultiArg.QuickCheckHelpers
  ( randSet, randText )
import Control.Monad ( liftM, liftM2 )

class Error e where
  parseErr :: Expecting -> Saw -> e

data SimpleError = SimpleError Expecting Saw deriving (Show, Eq)
printError :: SimpleError -> Text
printError (SimpleError e s) =
  pack "Error parsing command line input.\n"
  `append` pack "expected to see: "
  `append` printExpecting e `snoc` '\n'
  `append` pack "actually saw: "
  `append` printSaw s `snoc` '\n'

instance Error SimpleError where
  parseErr = SimpleError

instance Arbitrary SimpleError where
  arbitrary = liftM2 SimpleError arbitrary arbitrary

data Expecting = ExpPendingShortOpt ShortOpt
               | ExpExactLong LongOpt
               | ExpApproxLong (Set LongOpt)
               | ExpLongOptArg
               | ExpPendingShortArg
               | ExpStopper
               | ExpNextArg
               | ExpNonOptionPosArg
               | ExpEnd
               | ExpNonGNUExactLong LongOpt
               | ExpMatchingApproxLong LongOpt (Set LongOpt)
               | ExpNonGNUMatchingApproxLong LongOpt (Set LongOpt)
               | ExpApproxWord (Set Text)
               | ExpOptionOrPosArg
               | ExpTextError Text
               | ExpNonPendingShortOpt ShortOpt
               | ExpOtherFailure
               deriving (Show, Eq)

printExpecting :: Expecting -> Text
printExpecting e = case e of
  (ExpPendingShortOpt s) ->
    (pack "short option: ") `append` (singleton . unShortOpt $ s)
  (ExpExactLong l) ->
    (pack "long option: ") `append` (unLongOpt $ l)
  (ExpApproxLong ls) ->
    (pack "approximate long option matching one of: ") `append`
    intercalate (pack ", ") (map unLongOpt . Set.toList $ ls)
  ExpLongOptArg -> pack "argument to long option"
  ExpPendingShortArg -> pack "argument to short option"
  ExpStopper -> pack "stopper (\"--\")"
  ExpNextArg -> pack "next word on command line"
  ExpNonOptionPosArg ->
    pack "word on command line not starting with a hyphen"
  ExpEnd -> pack "end of command line input"
  (ExpNonGNUExactLong lo) ->
    pack "long option without an included argument: "
    `append` (unLongOpt lo)
  (ExpMatchingApproxLong l ls) ->
    pack "abbreviated long option named: " `append` (unLongOpt l)
    `append` pack "from possible abbreviated long options named: "
    `append` (intercalate (pack ", ")
              (map unLongOpt . Set.toList $ ls))
  (ExpNonGNUMatchingApproxLong l ls) ->
    pack "abbreviated long without an included argument named: "
    `append` (unLongOpt l)
    `append` pack "from possible abbreviated long options named: "
    `append` (intercalate (pack ", ")
              (map unLongOpt . Set.toList $ ls))
  (ExpApproxWord ws) ->
    pack "one of these abbreviated words: "
    `append` (intercalate (pack ", ") (Set.toList $ ws))
  ExpOptionOrPosArg ->
    pack "option or positional argument"
  (ExpTextError t) -> t
  (ExpNonPendingShortOpt s) ->
    (pack "short option: ") `append` (singleton . unShortOpt $ s)
  (ExpOtherFailure) -> pack "general failure"


instance Arbitrary Expecting where
  arbitrary = do
    i <- choose (0, (14 :: Int))
    case i of
      0 -> liftM ExpPendingShortOpt arbitrary
      1 -> liftM ExpExactLong arbitrary
      2 -> liftM ExpApproxLong randSet
      3 -> return ExpLongOptArg
      4 -> return ExpPendingShortArg
      5 -> return ExpStopper
      6 -> return ExpNextArg
      7 -> return ExpNonOptionPosArg
      8 -> return ExpEnd
      9 -> liftM ExpNonGNUExactLong arbitrary
      10 -> liftM2 ExpMatchingApproxLong arbitrary randSet
      11 -> liftM2 ExpNonGNUMatchingApproxLong arbitrary randSet
      12 -> liftM ExpApproxWord
            (liftM (Set.fromList . map pack) arbitrary)
      13 -> return ExpOptionOrPosArg
      14 -> liftM ExpNonPendingShortOpt arbitrary
      _  -> error "should never happen"

data Saw = SawNoPendingShorts
         | SawWrongPendingShort Char
         | SawNoArgsLeft
         | SawEmptyArg
         | SawSingleDashArg
         | SawStillPendingShorts TextNonEmpty
         | SawNotShortArg Text
         | SawWrongShortArg Char
         | SawNotLongArg Text
         | SawWrongLongArg Text
         | SawNoMatches Text
         | SawMultipleMatches (Set LongOpt) Text
         | SawNoPendingShortArg
         | SawAlreadyStopper
         | SawNewStopper
         | SawNotStopper
         | SawLeadingDashArg Text
         | SawMoreInput
         | SawGNULongOptArg Text
         | SawNotMatchingApproxLong Text LongOpt
         | SawMatchingApproxLongWithArg Text -- Text of the argument
         | SawMultipleApproxMatches (Set Text) Text
         | SawNoOption
         | SawNoOptionOrPosArg
         | SawTextError Text
         | SawOtherFailure
         deriving (Show, Eq)

printSaw :: Saw -> Text
printSaw s = case s of
  SawNoPendingShorts -> pack "no pending short options"
  (SawWrongPendingShort c) ->
    pack "unexpected short option: " `snoc` c
  SawNoArgsLeft -> pack "no command line words remaining"
  SawEmptyArg -> pack "command line word that is the empty string"
  SawSingleDashArg ->
    pack "command line word that is a single hyphen (\"-\")"
  (SawStillPendingShorts (TextNonEmpty first rest)) ->
    pack "pending short options: " `snoc` first
    `append` rest
  (SawNotShortArg t) ->
    pack "word that is not a short option: " `append` t
  (SawWrongShortArg c) ->
    pack "wrong short option: " `snoc` c
  (SawNotLongArg t) ->
    pack "word that is not a long option: " `append` t
  (SawWrongLongArg t) ->
    pack "wrong long option: " `append` t
  (SawNoMatches t) ->
    pack "word that does not match the available choices: "
    `append` t
  (SawMultipleMatches ss t) ->
    pack "word matches more than one of the available choices. "
    `append` pack "word given: " `append` t
    `append` pack " matches these words: "
    `append` (intercalate (pack ", ") (map unLongOpt . Set.toList $ ss))
  SawNoPendingShortArg -> pack "no short argument"
  SawAlreadyStopper ->
    pack "already seen a stopper (\"--\")"
  SawNewStopper ->
    pack "new stopper (\"--\")"
  SawNotStopper ->
    pack "word that is not a stopper (\"--\")"
  (SawLeadingDashArg t) ->
    pack "word with a leading hyphen: " `append` t
  SawMoreInput ->
    pack "additional words remaining on command line"
  (SawGNULongOptArg t) ->
    pack "attached argument for option that does not take one: "
    `append` t
  (SawNotMatchingApproxLong t lo) ->
    pack "long argument that does not match expected one. "
    `append` pack "argument given: " `append` t
    `append` pack "argument expected: " `append` unLongOpt lo
  (SawMatchingApproxLongWithArg t) ->
    pack "long argument that matches expected long argument, but it "
    `append` pack "has an attached argument. Text of argument: "
    `append` t
  (SawMultipleApproxMatches ms m) ->
    pack "multiple words match the one given. Word given: " `append` m
    `append` pack "possible matches: "
    `append` (intercalate (pack ", ") (Set.toList ms))
  SawNoOption ->
    pack "word that is not an option"
  SawNoOptionOrPosArg ->
    pack "not an option or positional argument"
  (SawTextError t) -> t
  (SawOtherFailure) -> pack "general failure"

instance Arbitrary Saw where
  arbitrary = do
    i <- choose (0, (23 :: Int))
    case i of
      0 -> return SawNoPendingShorts
      1 -> liftM SawWrongPendingShort arbitrary
      2 -> return SawNoArgsLeft
      3 -> return SawEmptyArg
      4 -> return SawSingleDashArg
      5 -> liftM SawStillPendingShorts arbitrary
      6 -> liftM SawNotShortArg randText
      7 -> liftM SawWrongShortArg arbitrary
      8 -> liftM SawNotLongArg randText
      9 -> liftM SawWrongLongArg randText
      10 -> liftM SawNoMatches randText
      11 -> liftM2 SawMultipleMatches randSet randText
      12 -> return SawNoPendingShortArg
      13 -> return SawAlreadyStopper
      14 -> return SawNewStopper
      15 -> return SawNotStopper
      16 -> liftM SawLeadingDashArg randText
      17 -> return SawMoreInput
      18 -> liftM SawGNULongOptArg randText
      19 -> liftM2 SawNotMatchingApproxLong randText arbitrary
      20 -> liftM SawMatchingApproxLongWithArg randText
      21 -> liftM2 SawMultipleApproxMatches
            (liftM (Set.fromList . map pack) arbitrary)
            randText
      22 -> return SawNoOption
      23 -> return SawNoOptionOrPosArg
      _  -> error "should never happen"
