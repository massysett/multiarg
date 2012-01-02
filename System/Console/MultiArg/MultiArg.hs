module System.Console.MultiArg.MultiArg where

import qualified Control.Monad.Exception.Synchronous as E
import Data.Text ( Text, pack, unpack, isPrefixOf )
import qualified Data.Text as X
import qualified Data.Set as Set
import Data.Set ( Set )
import Control.Monad ( when )

data Expecting = ExpCharOpt Char
                 | ExpExactLong Text
                 | ExpApproxLong (Set Text)
                 deriving Show

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
         | SawMultipleMatches (Set Text) Text
           

class Error e where
  store :: Text -> e
  unexpected :: Expecting -> Saw -> e

data Failed e a = Good a
              | Failed e

data TextNonEmpty = TextNonEmpty Char Text
                    deriving (Show, Eq)

textHead :: Text -> Maybe (Char, Text)
textHead t = case X.null t of
  True -> Nothing
  False -> Just (X.head t, X.tail t)

toTextNonEmpty :: Text -> Maybe TextNonEmpty
toTextNonEmpty t = case textHead t of
  Nothing -> Nothing
  (Just (c, r)) -> Just $ TextNonEmpty c r

data ParseSt = ParseSt { pendingShort :: Maybe TextNonEmpty
                       , remaining :: [Text]
                       } deriving (Show, Eq)

newtype Parser e a = Parser (ParseSt -> (Failed e a, ParseSt))

instance Monad (Parser e) where
  return a = Parser (\st -> (Good a, st))
  (Parser l) >>= f = Parser $ \s ->
    let (a1, s') = l s
    in case a1 of
      (Good g) -> let
        (Parser r) = f g
        in r s'
      (Failed t) -> (Failed t, s')
  fail _ = error "do not use fail in the Parser monad"

(<|>) :: Parser e a -> Parser e a -> Parser e a
(<|>) (Parser l) (Parser r) = Parser $ \s ->
  let (a1, s') = l s
  in case a1 of
    (Good g) -> (a1, s')
    (Failed t) -> case s' == s of
      True -> r s
      False -> (a1, s')

(<?>) :: Parser e a -> e -> Parser e a
(<?>) (Parser l) e = Parser $ \s ->
  let (a1, s') = l s
  in case a1 of
    (Good g) -> (Good g, s')
    (Failed e') -> case s' == s of
      True -> (Failed e', s')
      False -> (Failed e, s')

pendingShortOpt :: (Error e) => Char -> Parser e Char
pendingShortOpt c = Parser $ \s ->
  let err saw = ((Failed (unexpected (ExpCharOpt c) saw)), s)
      good st = (Good c, st)
  in E.switch err good $ do
    (TextNonEmpty first rest) <- case pendingShort s of
      Nothing -> E.throw SawNoPendingShorts
      (Just tne) -> return tne
    case c == first of
      False -> E.throw $ SawWrongPendingShort first
      True -> return s { pendingShort = toTextNonEmpty rest }

nonPendingShortOpt :: (Error e) => Char -> Parser e Char
nonPendingShortOpt c = Parser $ \s ->
  let err saw = ((Failed (unexpected (ExpCharOpt c) saw)), s)
      good st = (Good c, st)
  in E.switch err good $ do
    case pendingShort s of
      (Just ps) -> E.throw $ SawStillPendingShorts ps
      Nothing -> return ()
    (a:as) <- case remaining s of
      [] -> E.throw SawNoArgsLeft
      x -> return x
    (maybeDash, word) <- case textHead a of
      Nothing -> E.throw SawEmptyArg
      (Just w) -> return w
    when (maybeDash /= '-') $ E.throw (SawNotShortArg a)
    (letter, arg) <- case textHead word of
      Nothing -> E.throw SawSingleDashArg
      (Just w) -> return w
    when (letter /= c) $ E.throw (SawWrongShortArg letter)
    return s { pendingShort = toTextNonEmpty arg
             , remaining = as }

data LongOptGroup = LongOptGroup Text (Set Text)

exactLongOpt :: (Error e) => Text -> Parser e Text
exactLongOpt t = Parser $ \s -> let
  err saw = ((Failed (unexpected (ExpExactLong t) saw)), s)
  good st = (Good t, st)
  in E.switch err good $ do
    case pendingShort s of
      (Just ps) -> E.throw $ SawStillPendingShorts ps
      Nothing -> return ()
    (x:xs) <- case remaining s of
      [] -> E.throw SawNoArgsLeft
      ls -> return ls
    let (pre, suf) = X.splitAt 2 x
    when (pre /= pack "--") $ E.throw (SawNotLongArg x)
    when (suf /= t) $ E.throw (SawWrongLongArg suf)
    return s { remaining = xs }

-- | Examines the next word. If it is a non-GNU long option, and it
-- matches a Text in the set unambiguously, returns a tuple of the
-- word actually found and the matching word in the set.
approxLongOpt :: (Error e) => Set Text -> Parser e (Text, Text)
approxLongOpt ts = Parser $ \s -> let
  err saw = ((Failed (unexpected (ExpApproxLong ts) saw)), s)
  good (found, match, st) = (Good (found, match), st)
  in E.switch err good $ do
    case pendingShort s of
      (Just ps) -> E.throw $ SawStillPendingShorts ps
      Nothing -> return ()
    (x:xs) <- case remaining s of
      [] -> E.throw SawNoArgsLeft
      r -> return r
    let (pre, suf) = X.splitAt 2 x
    when (pre /= pack "--") (E.throw (SawNotLongArg x))
    let p t = suf `isPrefixOf` t
        matches = Set.filter p ts
    case Set.toList matches of
      [] -> E.throw (SawNoMatches suf)
      (m:[]) -> let
        st' = s { remaining = xs }
        in return (suf, m, st')
      ms -> E.throw (SawMultipleMatches matches suf)
