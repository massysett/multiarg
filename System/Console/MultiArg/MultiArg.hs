module System.Console.MultiArg.MultiArg where

import qualified Control.Monad.Trans.State.Lazy as St
import Data.Text ( Text, pack, unpack, isPrefixOf )
import qualified Data.Text as X
import qualified Data.Set as Set
import Data.Set ( Set )

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
pendingShortOpt c = Parser f where
  f s = let
    err saw = ((Failed (unexpected (ExpCharOpt c) saw)), s) in
    case pendingShort s of
      Nothing -> err SawNoPendingShorts
      (Just (TextNonEmpty first rest)) ->
        case c == first of
          False -> err (SawWrongPendingShort first)
          True -> let
            newSt = s { pendingShort = toTextNonEmpty rest }
            in (Good c, newSt)


nonPendingShortOpt :: (Error e) => Char -> Parser e Char
nonPendingShortOpt c = Parser $ \s -> let
  err saw = ((Failed (unexpected (ExpCharOpt c) saw)), s) in
  case pendingShort s of
    (Just ps) -> err (SawStillPendingShorts ps)
    Nothing -> case remaining s of
      [] -> err SawNoArgsLeft
      (a:as) -> case textHead a of
        Nothing -> err SawEmptyArg
        (Just (maybeDash, word)) -> case maybeDash == '-' of
          False -> err $ SawNotShortArg a
          True -> case textHead word of
            Nothing -> err SawSingleDashArg
            (Just (letter, arg)) -> case letter == c of
              False -> err $ SawWrongShortArg c
              True -> let
                newSt = s { pendingShort = toTextNonEmpty arg
                          , remaining = as }
                in (Good c, newSt)

data LongOptGroup = LongOptGroup Text (Set Text)

exactLongOpt :: (Error e) => Text -> Parser e Text
exactLongOpt t = Parser $ \s -> let
  err saw = ((Failed (unexpected (ExpExactLong t) saw)), s) in
  case pendingShort s of
    (Just ps) -> err $ SawStillPendingShorts ps
    Nothing -> case remaining s of
      [] -> err SawNoArgsLeft
      (x:xs) -> let
        (pre, suf) = X.splitAt 2 x
        in case pre == pack "--" of
          False -> err $ SawNotLongArg x
          True -> case suf == t of
            False -> err $ SawWrongLongArg suf
            True -> let
              s' = s { remaining = xs }
              in (Good t, s')

-- | Examines the next word. If it is a non-GNU long option, and it
-- matches a Text in the set unambiguously, returns a tuple of the
-- word actually found and the matching word in the set.
approxLongOpt :: (Error e) => Set Text -> Parser e (Text, Text)
approxLongOpt ts = Parser $ \s -> let
  err saw = ((Failed (unexpected (ExpApproxLong ts) saw)), s) in
  case pendingShort s of
    (Just ps) -> err $ SawStillPendingShorts ps
    Nothing -> case remaining s of
      [] -> err SawNoArgsLeft
      (x:xs) -> let
        (pre, suf) = X.splitAt 2 x
        in case pre == pack "--" of
          False -> err $ SawNotLongArg x
          True -> let
            p t = suf `isPrefixOf` t
            matches = Set.filter p ts
            in case Set.null matches of
              True -> err $ SawNoMatches suf
              False -> case Set.size matches > 1 of
                True -> err $ SawMultipleMatches matches suf
                False -> let
                  s' = s { remaining = xs }
                  r = (suf, head . Set.toList $ matches)
                  in (Good r, s')

