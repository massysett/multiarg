module Multiarg.Machine.State where

data T a
  = Empty
  -- ^ Accepting new tokens

  | Pending (String -> T a)
  -- ^ In the middle of processing an option; this function will be
  -- applied to the next token to get a result

  | Stopped
  -- ^ A stopper has been seen; additional tokens will pass through
