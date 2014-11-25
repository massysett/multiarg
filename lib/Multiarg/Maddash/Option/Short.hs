module Multiarg.Maddash.Option.Short where

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)
