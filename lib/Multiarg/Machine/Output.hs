module Multiarg.Machine.Output where

import qualified Multiarg.Machine.Error as Error

data T a
  = Good a
  | Error Error.T
  deriving (Eq, Ord, Show)
