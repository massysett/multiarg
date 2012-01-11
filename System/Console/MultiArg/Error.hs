module System.Console.MultiArg.Error where

import System.Console.MultiArg.Option
  ( LongOpt, ShortOpt )
import System.Console.MultiArg.TextNonEmpty ( TextNonEmpty )
import Data.Text ( Text, pack )
import Data.Set ( Set )
import qualified Data.Set as Set
import Test.QuickCheck ( Arbitrary ( arbitrary ), 
                         choose )
import System.Console.MultiArg.QuickCheckHelpers
  ( WText (WText), unWText, randSet, randText )
import Control.Monad ( liftM, liftM2 )

class Error e where
  unexpected :: Expecting -> Saw -> e

data SimpleError = SimpleError Expecting Saw deriving (Show, Eq)
printError :: SimpleError -> Text
printError = undefined

instance Error SimpleError where
  unexpected = SimpleError

instance Arbitrary SimpleError where
  arbitrary = liftM2 SimpleError arbitrary arbitrary

data Expecting = ExpPendingShortOpt ShortOpt
               | ExpNonPendingShortOpt ShortOpt
               | ExpExactLong LongOpt
               | ExpApproxLong (Set LongOpt)
               | ExpLongOptArg
               | ExpPendingShortArg
               | ExpStopper
               | ExpNextArg
               | ExpNonOptionPosArg
               | ExpEnd
               | ExpNonGNUExactLong LongOpt
               | ExpNonGNUApproxLong (Set LongOpt)
               | ExpMatchingApproxLong LongOpt (Set LongOpt)
               | ExpNonGNUMatchingApproxLong LongOpt (Set LongOpt)
               | ExpApproxWord (Set Text)
               | ExpOption Text
               | ExpOptionOrPosArg
               | ExpTextError Text
               deriving (Show, Eq)

printExpecting :: Expecting -> Text
printExpecting = undefined

instance Arbitrary Expecting where
  arbitrary = do
    i <- choose (0, (16 :: Int))
    case i of
      0 -> liftM ExpPendingShortOpt arbitrary
      16 -> liftM ExpNonPendingShortOpt arbitrary
      1 -> liftM ExpExactLong arbitrary
      2 -> liftM ExpApproxLong randSet
      3 -> return ExpLongOptArg
      4 -> return ExpPendingShortArg
      5 -> return ExpStopper
      6 -> return ExpNextArg
      7 -> return ExpNonOptionPosArg
      8 -> return ExpEnd
      9 -> liftM ExpNonGNUExactLong arbitrary
      10 -> liftM ExpNonGNUApproxLong randSet
      11 -> liftM2 ExpMatchingApproxLong arbitrary randSet
      12 -> liftM2 ExpNonGNUMatchingApproxLong arbitrary randSet
      13 -> liftM ExpApproxWord
            (liftM (Set.fromList . map pack) arbitrary)
      14 -> liftM ExpOption randText
      15 -> return ExpOptionOrPosArg
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
         | SawPendingLong Text
         | SawNoPendingLongArg
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
         deriving (Show, Eq)

printSaw :: Saw -> Text
printSaw = undefined

instance Arbitrary Saw where
  arbitrary = do
    i <- choose (0, (25 :: Int))
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
      12 -> liftM SawPendingLong randText
      13 -> return SawNoPendingLongArg
      14 -> return SawNoPendingShortArg
      15 -> return SawAlreadyStopper
      16 -> return SawNewStopper
      17 -> return SawNotStopper
      18 -> liftM SawLeadingDashArg randText
      19 -> return SawMoreInput
      20 -> liftM SawGNULongOptArg randText
      21 -> liftM2 SawNotMatchingApproxLong randText arbitrary
      22 -> liftM SawMatchingApproxLongWithArg randText
      23 -> liftM2 SawMultipleApproxMatches
            (liftM (Set.fromList . map pack) arbitrary)
            randText
      24 -> return SawNoOption
      25 -> return SawNoOptionOrPosArg
      _  -> error "should never happen"
