module Multiarg.Maddash.State where

import qualified Multiarg.Maddash.Output as Output

data T a
  = Empty
  -- ^ Accepting new tokens

  | Pending (String -> ([Output.T a], T a))
  -- ^ In the middle of processing an option; this function will be
  -- applied to the next token to get a result
