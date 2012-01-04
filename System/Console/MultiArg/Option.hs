module System.Console.MultiArg.Option where

import Test.QuickCheck ( Arbitrary ( arbitrary ),
                         suchThat, Gen )
import qualified Data.Text as X
import Data.Text ( Text, pack, unpack, isPrefixOf, cons )
import Control.Monad ( when )

newtype ShortOpt = ShortOpt Char deriving (Show, Eq)
instance Arbitrary ShortOpt where
  arbitrary = do
    c <- suchThat arbitrary (/= '-')
    return $ ShortOpt c

randText :: Gen Text
randText = do
  s <- arbitrary
  return (pack s)

makeShortOpt :: Char -> ShortOpt
makeShortOpt c = case c of
  '-' -> error "short option must not be a dash"
  x -> ShortOpt x

data LongOpt = LongOpt Text deriving (Show, Eq, Ord)
instance Arbitrary LongOpt where
  arbitrary = do
    t <- suchThat randText isValidLongOptText
    return $ LongOpt t

makeLongOpt :: Text -> LongOpt
makeLongOpt t = case isValidLongOptText t of
  True -> LongOpt t
  False -> error $ "invalid long option: " ++ unpack t

isValidLongOptText :: Text -> Bool
isValidLongOptText t = maybe False (const True) $ do
  when (pack "-" `isPrefixOf` t) Nothing
  when (pack "--" `isPrefixOf` t) Nothing
  case X.find (== '=') t of
    (Just _) -> Nothing
    Nothing -> return ()
  when (X.null t) Nothing
  return ()

