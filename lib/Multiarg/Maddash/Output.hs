module Multiarg.Maddash.Output where

import qualified Multiarg.Maddash.Error as Error

data T a
  = Good a
  | Error Error.T
  deriving (Eq, Ord, Show)
