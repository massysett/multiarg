module Multiarg.Types where

import Multiarg.Maddash

data OptSpec a = OptSpec
  { shorts :: [Char]
  , longs :: [String]
  , spec :: ArgSpec a
  } deriving Show

instance Functor OptSpec where
  fmap f (OptSpec s l p) = OptSpec s l (fmap f p)

