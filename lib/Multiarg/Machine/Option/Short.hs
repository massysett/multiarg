module Multiarg.Machine.Option.Short where

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)
