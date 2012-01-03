{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Console.MultiArg.MultiArg where

import qualified Control.Monad.Exception.Synchronous as E
import Control.Applicative ( Applicative )
import Control.Monad.Exception.Synchronous
  ( ExceptionalT, runExceptionalT, throwT, Exceptional(Success, Exception) )
import Control.Monad.Trans.State.Lazy ( State, get, runState, put )
import qualified Control.Monad.Trans.State.Lazy as St
import Data.Text ( Text, pack, unpack, isPrefixOf, cons )
import qualified Data.Text as X
import qualified Data.Set as Set
import Data.Set ( Set )
import Control.Monad ( when )
import Control.Monad.Trans.Class ( lift )
import Test.QuickCheck ( Arbitrary ( arbitrary ),
                         suchThat )
import Text.Printf ( printf )
import Data.Maybe ( isNothing )

data Expecting = ExpCharOpt ShortOpt
                 | ExpExactLong LongOpt
                 | ExpApproxLong (Set LongOpt)
                 | ExpPendingLongOptArg
                 | ExpPendingShortArg
                 | ExpStopper
                 deriving (Show, Eq)

data Saw = SawNoPendingShorts
         | SawWrongPendingShort Char
         | SawNoArgsLeft
         | SawEmptyArg
         | SawSingleDashArg
         | SawStillPendingShorts TextNonEmpty
         | SawNotShortArg Text
         | SawWrongShortArg Char
         | SawNotLongArg Text
         | SawWrongLongArg Text
         | SawNoMatches Text
         | SawMultipleMatches (Set LongOpt) Text
         | SawPendingLong Text
         | SawNoPendingLongArg
         | SawNoPendingShortArg
         | SawAlreadyStopper
         | SawNewStopper
         | SawNotStopper
         deriving (Show, Eq)
           
newtype ShortOpt = ShortOpt Char deriving (Show, Eq)
instance Arbitrary ShortOpt where
  arbitrary = do
    c <- suchThat arbitrary (/= '-')
    return $ ShortOpt c

shortOpt :: Char -> ShortOpt
shortOpt c = case c of
  '-' -> error "short option must not be a dash"
  x -> ShortOpt x

instance Arbitrary Text where
  arbitrary = do
    s <- arbitrary
    return . pack $ s

data LongOpt = LongOpt Text deriving (Show, Eq, Ord)
instance Arbitrary LongOpt where
  arbitrary = do
    t <- suchThat arbitrary isValidLongOptText
    return $ LongOpt t

longOpt :: Text -> LongOpt
longOpt t = case isValidLongOptText t of
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

class Error e where
  unexpected :: Expecting -> Saw -> e

data SimpleError = SimpleError Expecting Saw deriving (Show, Eq)
instance Error SimpleError where unexpected = SimpleError

data TextNonEmpty = TextNonEmpty Char Text
                    deriving (Show, Eq)

instance Arbitrary TextNonEmpty where
  arbitrary = do
    c <- arbitrary
    t <- arbitrary
    return $ TextNonEmpty c t

textHead :: Text -> Maybe (Char, Text)
textHead t = case X.null t of
  True -> Nothing
  False -> Just (X.head t, X.tail t)

toTextNonEmpty :: Text -> Maybe TextNonEmpty
toTextNonEmpty t = case textHead t of
  Nothing -> Nothing
  (Just (c, r)) -> Just $ TextNonEmpty c r

data ParseSt s = ParseSt { pendingShort :: Maybe TextNonEmpty
                         , pendingLongArg :: Maybe Text
                         , remaining :: [Text]
                         , sawStopper :: Bool
                         , userState :: s
                         } deriving (Show, Eq)

newtype ParserSE s e a =
  ParserSE { runParser :: ExceptionalT e (State (ParseSt s)) a }
  deriving (Monad, Functor, Applicative)

type ParserE e a = ParserSE ()
type Parser a = ParserSE () SimpleError a

(<|>) :: ParserSE s e a -> ParserSE s e a -> ParserSE s e a
(<|>) (ParserSE l) (ParserSE r) = ParserSE $ do
  s <- lift get
  let (a1, s') = flip runState s . runExceptionalT $ l
  case a1 of
    (Success g) -> lift (put s') >> return g
    (Exception f) ->
      if noConsumed s s' then r
      else lift (put s') >> throwT f


noConsumed :: ParseSt st -> ParseSt st -> Bool
noConsumed = undefined

(<?>) :: ParserSE s e a -> e -> ParserSE s e a
(<?>) (ParserSE l) e = ParserSE $ do
  s <- lift get
  let (a1, s') = flip runState s . runExceptionalT $ l
  case a1 of
    (Success g) -> lift (put s') >> return g
    (Exception e') ->
      if noConsumed s s' then throwT e
      else lift (put s') >> throwT e'

pendingShortOpt :: (Error e) => ShortOpt -> ParserSE s e ShortOpt
pendingShortOpt so@(ShortOpt c) = ParserSE $ do
  s <- lift get
  let err saw = throwT (unexpected (ExpCharOpt so) saw)
  when (sawStopper s) (err SawAlreadyStopper)
  maybe (return ()) (err . SawPendingLong) (pendingLongArg s)
  (TextNonEmpty first rest) <-
    maybe (err SawNoPendingShorts) return (pendingShort s)
  when (c /= first) (err $ SawWrongPendingShort first)
  lift $ put s { pendingShort = toTextNonEmpty rest } 
  return so

nonPendingShortOpt :: (Error e) => ShortOpt -> ParserSE s e ShortOpt
nonPendingShortOpt so@(ShortOpt c) = ParserSE $ do
  let err saw = throwT (unexpected (ExpCharOpt so) saw)
  s <- lift get
  maybe (return ()) (err . SawStillPendingShorts) (pendingShort s)
  maybe (return ()) (err . SawPendingLong) (pendingLongArg s)
  when (sawStopper s) (err SawAlreadyStopper)
  (a:as) <- case remaining s of
    [] -> err SawNoArgsLeft
    x -> return x
  (maybeDash, word) <- case textHead a of
    Nothing -> err SawEmptyArg
    (Just w) -> return w
  when (maybeDash /= '-') $ err (SawNotShortArg a)
  (letter, arg) <- case textHead word of
    Nothing -> err SawSingleDashArg
    (Just w) -> return w
  when (letter /= c) $ err (SawWrongShortArg letter)
  when (letter == '-') $ err SawNewStopper
  lift $ put s { pendingShort = toTextNonEmpty arg
               , remaining = as }
  return so

