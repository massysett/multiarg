module System.Console.MultiArg.QuickCheckHelpers where

import Test.QuickCheck ( Arbitrary ( arbitrary ),
                         Gen )
import Data.Text ( Text, pack )

randText :: Gen Text
randText = do
  s <- arbitrary
  return (pack s)

