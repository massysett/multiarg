module Multiarg.Machine.Option.Long where

data T = T
  { toString :: String
  , abbreviate :: Bool
  } deriving (Eq, Ord, Show)
