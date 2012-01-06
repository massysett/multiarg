module System.Console.MultiArg.TextNonEmpty where

import Test.QuickCheck ( Arbitrary ( arbitrary ) )
import System.Console.MultiArg.QuickCheckHelpers ( randText )
import Data.Text ( Text )

data TextNonEmpty = TextNonEmpty Char Text
                    deriving (Show, Eq)

instance Arbitrary TextNonEmpty where
  arbitrary = do
    c <- arbitrary
    t <- randText
    return $ TextNonEmpty c t
