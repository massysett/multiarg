module System.Console.MultiArg.QuickCheckHelpers where

import Test.QuickCheck ( Arbitrary ( arbitrary ),
                         Gen,
                         CoArbitrary ( coarbitrary ),
                         coarbitraryShow )
import Data.Text ( Text, pack )
import qualified Data.Set as Set
import Control.Monad ( liftM )

randText :: Gen Text
randText = liftM pack arbitrary

randSet :: (Ord a, Arbitrary a) => Gen (Set.Set a)
randSet = liftM Set.fromList arbitrary

newtype WText = WText { unWText :: Text }
                deriving Show

instance Arbitrary WText where
  arbitrary = liftM WText randText

instance CoArbitrary WText where
  coarbitrary (WText a) gc = coarbitraryShow a gc
