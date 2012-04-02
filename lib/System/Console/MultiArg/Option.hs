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

import Test.QuickCheck ( Arbitrary ( arbitrary ),
                         suchThat )
import qualified Data.Text as X
import Data.Text ( Text, unpack, index )
import Control.Monad ( when )
import System.Console.MultiArg.QuickCheckHelpers ( randText )

-- | Short options. Options that are preceded with a single dash on
-- the command line and consist of a single letter. That single letter
-- cannot be a dash. Any other Unicode character is good (including
-- pathological ones like newlines).
newtype ShortOpt = ShortOpt { unShortOpt :: Char } deriving (Show, Eq, Ord)
instance Arbitrary ShortOpt where
  arbitrary = do
    c <- suchThat arbitrary (/= '-')
    return $ ShortOpt c

-- | This function is partial. It calls error if its argument is a
-- single dash. This is the only way to make a short option so it
-- prevents you from making one that is invalid.
makeShortOpt :: Char -> ShortOpt
makeShortOpt c = case c of
  '-' -> error "short option must not be a dash"
  x -> ShortOpt x

-- | Long options. Options that are preceded with two dashes on the
-- command line and typically consist of an entire mnemonic word, such
-- as @lines@. However, anything that is at least one letter long is
-- fine for a long option name. The name must not have a dash as
-- either the first or second character and it must be at least one
-- character long. It cannot have an equal sign anywhere in its
-- name. Otherwise any Unicode character is good (including
-- pathological ones like newlines).
data LongOpt = LongOpt { unLongOpt :: Text } deriving (Show, Eq, Ord)
instance Arbitrary LongOpt where
  arbitrary = do
    t <- suchThat randText isValidLongOptText
    return $ LongOpt t

-- | This function is partial. It calls error if its argument contains
-- text that is not a valid long option. This is the only way to make
-- a long option so it prevents you from making invalid ones.
makeLongOpt :: Text -> LongOpt
makeLongOpt t = case isValidLongOptText t of
  True -> LongOpt t
  False -> error $ "invalid long option: " ++ unpack t

isValidLongOptText :: Text -> Bool
isValidLongOptText t = maybe False (const True) $ do
  when (X.null t) Nothing
  when ((t `index` 0) == '-') Nothing
  when ((X.length t > 1) && ((t `index` 1) == '-')) Nothing
  case X.find (== '=') t of
    (Just _) -> Nothing
    Nothing -> return ()
  return ()


