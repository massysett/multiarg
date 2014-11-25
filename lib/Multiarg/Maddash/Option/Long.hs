module Multiarg.Maddash.Option.Long where

newtype T = T { toString :: String }
  deriving (Eq, Ord, Show)
