-- | These types represent options. Option names cannot have a dash as
-- their first or second character, and long option names cannot have
-- an equals sign anywhere in the name.
module Multiarg.Option (
  ShortOpt,
  unShortOpt,
  makeShortOpt,
  LongOpt,
  unLongOpt,
  makeLongOpt )
  where

-- | Short options. Options that are preceded with a single dash on
-- the command line and consist of a single letter. That single letter
-- cannot be a dash. Any other Unicode character is good (including
-- pathological ones like newlines).
newtype ShortOpt = ShortOpt { unShortOpt :: Char } deriving (Show, Eq, Ord)

-- | Creates a short option. Returns Nothing if the character is not
-- valid for a short option.
makeShortOpt :: Char -> Maybe ShortOpt
makeShortOpt c = case c of
  '-' -> Nothing
  x -> Just $ ShortOpt x

-- | Long options. Options that are preceded with two dashes on the
-- command line and typically consist of an entire mnemonic word, such
-- as @lines@. However, anything that is at least one letter long is
-- fine for a long option name. The name must be at least one
-- character long. It cannot have an equal sign anywhere in its
-- name. Otherwise any Unicode character is good (including
-- pathological ones like newlines).
data LongOpt = LongOpt { unLongOpt :: String } deriving (Show, Eq, Ord)

-- | Makes a long option. Returns Nothing if the string is not a valid
-- long option.
makeLongOpt :: String -> Maybe LongOpt
makeLongOpt t =
  if isValidLongOptText t then Just $ LongOpt t else Nothing


isValidLongOptText :: String -> Bool
isValidLongOptText s = case s of
  [] -> False
  xs -> not $ '=' `elem` xs
