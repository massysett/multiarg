module Multiarg.Machine.Arguments where

-- | Some of the functions return an @Either String a@.  In that case,
-- return a Left String to indicate an error, or a Right a to indicate
-- success.  The @String@ in the @Left String@ is an error message;
-- the faulty option is always indicated for you.  To supply
-- additional information in the rror, pass it in the @String@; if you
-- have no useful diagnostic information to add, just return an empty
-- @String@.
data T a
  = Zero a
  -- ^ This option takes no arguments
  | One (String -> Either String a)
  -- ^ This option takes one argument
  | Two (String -> String -> Either String a)
  -- ^ This option takes two arguments
  | Three (String -> String -> String -> Either String a)
  -- ^ This option takes three arguments
