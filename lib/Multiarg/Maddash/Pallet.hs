module Multiarg.Maddash.Pallet where

import qualified Multiarg.Maddash.Output as Output

data T a
  = NotAnOption
  | Full [Output.T a]
  deriving (Eq, Ord, Show)
