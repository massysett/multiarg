module System.Console.MultiArg.QuickCheckHelpers where

import Test.QuickCheck ( Arbitrary ( arbitrary ),
                         Gen,
                         CoArbitrary ( coarbitrary ),
                         coarbitraryShow )
import Data.Text ( Text, pack )

randText :: Gen Text
randText = do
  s <- arbitrary
  return (pack s)

newtype WText = WText { unWText :: Text }
                deriving Show

instance Arbitrary WText where
  arbitrary = do
    t <- randText
    return $ WText t

instance CoArbitrary WText where
  coarbitrary (WText a) gc = coarbitraryShow a gc
