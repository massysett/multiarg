-- | These types represent options. They are abstract and in a
-- separate module to prevent you from accidentally making an option
-- with an invalid name. Option names cannot have a dash as their
-- first or second character, and long option names cannot have an
-- equals sign anywhere in the name.
module System.Console.MultiArg.Option (
  ShortOpt,
  unShortOpt,
  makeShortOpt,
  LongOpt,
  unLongOpt,
  makeLongOpt )
  where

import Data.List (find)

-- | Short options. Options that are preceded with a single dash on
-- the command line and consist of a single letter. That single letter
-- cannot be a dash. Any other Unicode character is good (including
-- pathological ones like newlines).
newtype ShortOpt = ShortOpt { unShortOpt :: Char } deriving (Show, Eq, Ord)

-- | This function is partial. It calls error if its argument is a
-- single dash. This is the only way to make a short option so it
-- prevents you from making one that is invalid.
makeShortOpt :: Char -> Maybe ShortOpt
makeShortOpt c = case c of
  '-' -> Nothing
  x -> Just $ ShortOpt x

-- | Long options. Options that are preceded with two dashes on the
-- command line and typically consist of an entire mnemonic word, such
-- as @lines@. However, anything that is at least one letter long is
-- fine for a long option name. The name must not have a dash as
-- either the first or second character and it must be at least one
-- character long. It cannot have an equal sign anywhere in its
-- name. Otherwise any Unicode character is good (including
-- pathological ones like newlines).
data LongOpt = LongOpt { unLongOpt :: String } deriving (Show, Eq, Ord)

-- | This function is partial. It calls error if its argument contains
-- text that is not a valid long option. This is the only way to make
-- a long option so it prevents you from making invalid ones.
makeLongOpt :: String -> Maybe LongOpt
makeLongOpt t = case isValidLongOptText t of
  True -> Just $ LongOpt t
  False -> Nothing

isValidLongOptText :: String -> Bool
isValidLongOptText s = case s of
  [] -> False
  x:xs ->
    if x == '-' || x == '='
    then False
    else case xs of
      [] -> True
      y:_ ->
        if y == '-' || y == '='
        then False
        else maybe True (const False) (find (== '=') xs)
