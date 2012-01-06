module System.Console.MultiArg.TextNonEmpty where

import Test.QuickCheck
  ( Arbitrary ( arbitrary ), CoArbitrary ( coarbitrary ),
    (><), variant )
import System.Console.MultiArg.QuickCheckHelpers 
  ( randText, WText(WText), unWText )
                                                   
import Data.Text ( Text )

data TextNonEmpty = TextNonEmpty Char Text
                    deriving (Show, Eq)

instance Arbitrary TextNonEmpty where
  arbitrary = do
    c <- arbitrary
    t <- randText
    return $ TextNonEmpty c t

instance CoArbitrary TextNonEmpty where
  coarbitrary (TextNonEmpty c t)  = vc >< vt where
    vc = coarbitrary c
    vt = coarbitrary (WText t)
