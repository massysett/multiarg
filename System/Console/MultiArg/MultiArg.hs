module System.Console.MultiArg.MultiArg where

import qualified Control.Monad.Exception.Synchronous as E
import Data.Text ( Text, pack, unpack, isPrefixOf, singleton, cons )
import qualified Data.Text as X
import qualified Data.Set as Set
import Data.Set ( Set )
import Control.Monad ( when )
import Test.QuickCheck
import Text.Printf ( printf )

data Expecting = ExpCharOpt ShortOpt
                 | ExpExactLong LongOpt
                 | ExpApproxLong (Set LongOpt)
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
         deriving (Show, Eq)
           
class Error e where
  unexpected :: Expecting -> Saw -> e

data SimpleError = SimpleError Expecting Saw deriving (Show, Eq)
instance Error SimpleError where unexpected = SimpleError

data Failed e a = Good a
              | Failed e
              deriving (Show, Eq)

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

instance Arbitrary Text where
  arbitrary = do
    s <- arbitrary
    return . pack $ s

newtype ShortOpt = ShortOpt Char deriving (Show, Eq)
instance Arbitrary ShortOpt where
  arbitrary = do
    c <- suchThat arbitrary (/= '-')
    return $ ShortOpt c

shortOpt :: Char -> ShortOpt
shortOpt c = case c of
  '-' -> error "short option must not be a dash"
  x -> ShortOpt x

------------------------------------------------------------
-- pendingShortOpt and tests
------------------------------------------------------------

-- | Generates a list of Text where the first Text has a first letter
-- matching the one given.
matchingRemaining :: Char -> Gen [Text]
matchingRemaining c = do
  restWord <- arbitrary
  let firstWord = c `cons` restWord
  restWords <- arbitrary
  return $ firstWord : restWords

data NoPendingShorts = NoPendingShorts ParseSt ShortOpt deriving Show
instance Arbitrary NoPendingShorts where
  arbitrary = do
    o <- arbitrary
    let (ShortOpt c) = o
    r <- oneof [matchingRemaining c, arbitrary]
    return $ NoPendingShorts (ParseSt Nothing r) o

-- | If there are no pending short options, pendingShortOpt must
-- return SawNoPendingShorts and the input state must be unchanged.
prop_noPendingShorts :: NoPendingShorts -> Bool
prop_noPendingShorts (NoPendingShorts st so) = exp == actual where
  exp = (Failed $ SimpleError (ExpCharOpt so) SawNoPendingShorts, st)
  (Parser f) = pendingShortOpt so
  actual = f st


data WrongPendingShort = WrongPendingShort ParseSt ShortOpt
                         deriving Show
instance Arbitrary WrongPendingShort where
  arbitrary = do
    o <- arbitrary
    let (ShortOpt c) = o
    r <- oneof [matchingRemaining c, arbitrary]
    firstLetter <- suchThat arbitrary (/= c)
    rest <- arbitrary
    let st = ParseSt (Just (TextNonEmpty firstLetter rest)) r
    return $ WrongPendingShort st o

-- | If there is a pending short option, but it is the wrong one,
-- pendingShortOpt must retur SawWrongPendingShort and the input state
-- must be unchanged.
prop_WrongPendingShort :: WrongPendingShort -> Bool
prop_WrongPendingShort (WrongPendingShort st so) = exp == act where
  (ShortOpt c) = so
  exp = (Failed err, st) where
    err = SimpleError (ExpCharOpt so) (SawWrongPendingShort wrong)
    wrong = case pendingShort st of
      Nothing -> error "prop_WrongPendingShort error"
      (Just (TextNonEmpty ch _)) -> ch
  (Parser f) = pendingShortOpt so
  act = f st

data GoodPendingShort = GoodPendingShort ParseSt ShortOpt
                        deriving Show
instance Arbitrary GoodPendingShort where
  arbitrary = do
    o <- arbitrary
    let (ShortOpt firstLetter) = o
    restWord <- arbitrary
    restWords <- oneof [matchingRemaining firstLetter, arbitrary]
    let st = ParseSt (Just (TextNonEmpty firstLetter restWord)) restWords
    return $ GoodPendingShort st o

-- | If the pending short option is good, return the input pending
-- short option, and a state where the pending shorts are reduced but
-- the remaining options stay the same.

prop_goodPendingShort :: GoodPendingShort -> Bool
prop_goodPendingShort (GoodPendingShort st so) = exp == act where
  newPendingShort = toTextNonEmpty rest
  rest = case pendingShort st of
    Nothing -> error "prop_goodPendingShort error"
    (Just (TextNonEmpty _ rs)) -> rs
  rem = remaining st
  newSt = ParseSt newPendingShort rem
  exp = (Good so, newSt)
  (Parser f) = pendingShortOpt so
  act = f st :: (Failed SimpleError ShortOpt, ParseSt)

pendingShortOpt :: (Error e) => ShortOpt -> Parser e ShortOpt
pendingShortOpt so@(ShortOpt c) = Parser $ \s ->
  let err saw = ((Failed (unexpected (ExpCharOpt so) saw)), s)
      good st = (Good so, st)
  in E.switch err good $ do
    (TextNonEmpty first rest) <- case pendingShort s of
      Nothing -> E.throw SawNoPendingShorts
      (Just tne) -> return tne
    case c == first of
      False -> E.throw $ SawWrongPendingShort first
      True -> return s { pendingShort = toTextNonEmpty rest }

------------------------------------------------------------
------------------------------------------------------------

------------------------------------------------------------
-- nonPendingShortOpt and tests
------------------------------------------------------------
nonPendingShortOpt :: (Error e) => ShortOpt -> Parser e ShortOpt
nonPendingShortOpt so@(ShortOpt c) = Parser $ \s ->
  let err saw = ((Failed (unexpected (ExpCharOpt so) saw)), s)
      good st = (Good so, st)
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

exactLongOpt :: (Error e) => LongOpt -> Parser e LongOpt
exactLongOpt lo@(LongOpt t) = Parser $ \s -> let
  err saw = ((Failed (unexpected (ExpExactLong lo) saw)), s)
  good st = (Good lo, st)
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
approxLongOpt :: (Error e) => Set LongOpt -> Parser e (Text, LongOpt)
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
    let p (LongOpt t) = suf `isPrefixOf` t
        matches = Set.filter p ts
    case Set.toList matches of
      [] -> E.throw (SawNoMatches suf)
      (m:[]) -> let
        st' = s { remaining = xs }
        in return (suf, m, st')
      ms -> E.throw (SawMultipleMatches matches suf)

tests :: [(String, IO ())]
tests = [ ("prop_noPendingShorts", quickCheck prop_noPendingShorts)
        , ("prop_WrongPendingShort", quickCheck prop_WrongPendingShort)
        , ("prop_goodPendingShort", quickCheck prop_goodPendingShort)
        ]

main :: IO ()
main = mapM_ (\(s, a) -> printf "%-25s: " s >> a) tests
