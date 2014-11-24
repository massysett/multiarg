module Multiarg.Machine.Option.Long where

newtype T = T { toString :: String }
  deriving (Eq, Ord, Show)
