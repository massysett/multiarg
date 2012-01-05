module System.Console.MultiArg.Error where

import System.Console.MultiArg.Option
  ( LongOpt, ShortOpt, TextNonEmpty )
import Data.Text ( Text )
import Data.Set ( Set )

class Error e where
  unexpected :: Expecting -> Saw -> e
  changeExpecting :: Expecting -> e -> e

data SimpleError = SimpleError Expecting Saw deriving (Show, Eq)
instance Error SimpleError where
  unexpected = SimpleError
  changeExpecting exp' (SimpleError exp s) = SimpleError exp' s

data Expecting = ExpCharOpt ShortOpt
                 | ExpExactLong LongOpt
                 | ExpApproxLong (Set LongOpt)
                 | ExpPendingLongOptArg
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
                 deriving (Show, Eq)

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
         deriving (Show, Eq)
