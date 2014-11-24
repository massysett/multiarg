module Multiarg.Machine.Error where

data T
  = BadShortOption Char
  | BadArgument1 String String
  -- ^ The first String is the bad argument; the second String is any
  -- error message.
  | BadArgument2 String String String
  | BadArgument3 String String String String
  | LongOptionNotFound String
  | AmbiguousLongOption String [String]
  deriving (Eq, Ord, Show)
