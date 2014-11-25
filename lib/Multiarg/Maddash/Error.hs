module Multiarg.Maddash.Error where

data T
  = BadShortOption Char
  | BadArgument1 String String
  -- ^ The first String is the bad argument; the second String is any
  -- error message.
  | BadArgument2 String String String
  | BadArgument3 String String String String
  | LongOptionNotFound String
  | LongArgumentForZeroArgumentOption String String
  -- ^ The uesr gave an argument for a long option that does not take
  -- an argument.  The first String is the name of the long option;
  -- the second String is the argument given.
  deriving (Eq, Ord, Show)
