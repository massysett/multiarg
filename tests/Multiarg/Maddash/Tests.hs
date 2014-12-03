module Multiarg.Maddash.Tests where

import Multiarg.Maddash
import Test.QuickCheck

genShort :: Gen Short
genShort = fmap Short arbitrary

genLong :: Gen Long
genLong = fmap Long arbitrary

prop_alwaysTrue :: Bool
prop_alwaysTrue = True
