module System.Console.MultiArg.MultiArg where

import qualified Control.Monad.Trans.State.Lazy as St
import Data.Text ( Text, pack, unpack, isPrefixOf )
import qualified Data.Text as X
import qualified Data.Set as Set
import Data.Set ( Set )

class Error e where
  store :: Text -> e
  noPendingShortOpts :: Char -> e
  wrongPendingShortOpt :: Char -> Char -> e
  nonPendingShortNoArgsLeft :: Char -> e
  nonPendingShortEmptyArg :: e
  nonPendingShortSingleDashOnly :: Char -> e
  nonPendingShortNotShortArg :: Char -> Text -> e
  nonPendingShortWrongLetter :: Char -> Char -> e
  nonPendingShortStillPendingArgs :: Char -> TextNonEmpty -> e
  exactLongPendingShorts :: Text -> TextNonEmpty -> e
  exactLongNoArgsLeft :: Text -> e
  exactLongNextNotLong :: Text -> e
  exactLongNonMatch :: Text -> Text -> e
  approxLongPendingShorts :: Set Text -> TextNonEmpty -> e
  approxLongNextNotLong :: Set Text -> Text -> e
  approxLongNoArgsLeft :: Set Text -> e
  approxLongNoMatches :: Set Text -> Text -> e
  approxLongMultipleMatches :: Set Text -> Text -> e

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
  f s = case pendingShort s of
    Nothing -> (Failed $ noPendingShortOpts c, s)
    (Just (TextNonEmpty first rest)) ->
      case c == first of
        True -> let
          newSt = s { pendingShort = toTextNonEmpty rest }
          in (Good c, newSt)
        False -> (Failed $ wrongPendingShortOpt c first, s)

nonPendingShortOpt :: (Error e) => Char -> Parser e Char
nonPendingShortOpt c = Parser $ \s ->
  case pendingShort s of
    (Just ps) -> (Failed $ nonPendingShortStillPendingArgs c ps, s)
    Nothing -> case remaining s of
      [] -> (Failed $ nonPendingShortNoArgsLeft c, s)
      (a:as) -> case textHead a of
        Nothing -> (Failed $ nonPendingShortEmptyArg, s)
        (Just (maybeDash, word)) -> case maybeDash == '-' of
          False -> (Failed $ nonPendingShortNotShortArg c a, s)
          True -> case textHead word of
            Nothing -> (Failed $ nonPendingShortSingleDashOnly c, s)
            (Just (letter, arg)) -> case letter == c of
              False -> (Failed $ nonPendingShortWrongLetter c letter, s)
              True -> let
                newSt = s { pendingShort = toTextNonEmpty arg
                          , remaining = as }
                in (Good c, newSt)

data LongOptGroup = LongOptGroup Text (Set Text)

exactLongOpt :: (Error e) => Text -> Parser e Text
exactLongOpt t = Parser $ \s -> case pendingShort s of
  (Just ps) -> (Failed $ exactLongPendingShorts t ps, s)
  Nothing -> case remaining s of
    [] -> (Failed $ exactLongNoArgsLeft t, s)
    (x:xs) -> let
      (pre, suf) = X.splitAt 2 x
      in case pre == pack "--" of
        False -> (Failed $ exactLongNextNotLong x, s)
        True -> case suf == t of
          False -> (Failed $ exactLongNonMatch t x, s)
          True -> let
            s' = s { remaining = xs }
            in (Good t, s')

-- | Examines the next word. If it is a non-GNU long option, and it
-- matches a Text in the set unambiguously, returns a tuple of the
-- word actually found and the matching word in the set.
approxLongOpt :: (Error e) => Set Text -> Parser e (Text, Text)
approxLongOpt ts = Parser $ \s -> case pendingShort s of
  (Just ps) -> (Failed $ approxLongPendingShorts ts ps, s)
  Nothing -> case remaining s of
    [] -> (Failed $ approxLongNoArgsLeft ts, s)
    (x:xs) -> let
      (pre, suf) = X.splitAt 2 x
      in case pre == pack "--" of
        False -> (Failed $ approxLongNextNotLong ts x, s)
        True -> let
          p t = suf `isPrefixOf` t
          matches = Set.filter p ts
          in case Set.null matches of
            True -> (Failed $ approxLongNoMatches ts x, s)
            False -> case Set.size matches > 1 of
              True -> (Failed $ approxLongMultipleMatches ts x, s)
              False -> let
                s' = s { remaining = xs }
                r = (suf, head . Set.toList $ matches)
                in (Good r, s')

