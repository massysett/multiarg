{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Console.MultiArg.MultiArg where

import qualified Control.Monad.Exception.Synchronous as E
import Data.Text ( Text, pack, unpack, isPrefixOf, cons )
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
         | SawPendingLong Text
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
                       , pendingLongArg :: Maybe Text
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
    (Good _) -> (a1, s')
    (Failed _) -> case s' == s of
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
    l <- arbitrary
    return $ NoPendingShorts (ParseSt Nothing l r) o

-- | If there are no pending short options, pendingShortOpt must
-- return SawNoPendingShorts and the input state must be unchanged.
prop_noPendingShorts :: NoPendingShorts -> Bool
prop_noPendingShorts (NoPendingShorts st so) = ex == actual where
  ex = (Failed $ SimpleError (ExpCharOpt so) SawNoPendingShorts, st)
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
    l <- arbitrary
    let st = ParseSt (Just (TextNonEmpty firstLetter rest)) l r
    return $ WrongPendingShort st o

-- | If there is a pending short option, but it is the wrong one,
-- pendingShortOpt must retur SawWrongPendingShort and the input state
-- must be unchanged.
prop_WrongPendingShort :: WrongPendingShort -> Bool
prop_WrongPendingShort (WrongPendingShort st so) = ex == act where
  ex = (Failed err, st) where
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
    l <- arbitrary
    let st = ParseSt (Just (TextNonEmpty firstLetter restWord)) l restWords
    return $ GoodPendingShort st o

-- | If the pending short option is good, return the input pending
-- short option, and a state where the pending shorts are reduced but
-- the remaining options stay the same.

prop_goodPendingShort :: GoodPendingShort -> Bool
prop_goodPendingShort (GoodPendingShort st so) = ex == act where
  newPendingShort = toTextNonEmpty rest
  rest = case pendingShort st of
    Nothing -> error "prop_goodPendingShort error"
    (Just (TextNonEmpty _ rs)) -> rs
  re = remaining st
  newSt = st { pendingShort = newPendingShort,
               remaining = re }
  ex = (Good so, newSt)
  (Parser f) = pendingShortOpt so
  act = f st :: (Failed SimpleError ShortOpt, ParseSt)

pendingShortOpt :: (Error e) => ShortOpt -> Parser e ShortOpt
pendingShortOpt so@(ShortOpt c) = Parser $ \s ->
  let err saw = ((Failed (unexpected (ExpCharOpt so) saw)), s)
      good st = (Good so, st)
  in E.switch err good $ do
    maybe (return ()) (E.throw . SawPendingLong) (pendingLongArg s)
    (TextNonEmpty first rest) <-
      maybe (E.throw SawNoPendingShorts) return (pendingShort s)
    when (c /= first) (E.throw $ SawWrongPendingShort first)
    return s { pendingShort = toTextNonEmpty rest }

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
    maybe (return ()) (E.throw . SawStillPendingShorts) (pendingShort s)
    maybe (return ()) (E.throw . SawPendingLong) (pendingLongArg s)
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
    maybe (return ()) (E.throw . SawPendingLong) (pendingLongArg s)
    maybe (return ()) (E.throw . SawStillPendingShorts) (pendingShort s)
    (x:xs) <- case remaining s of
      [] -> E.throw SawNoArgsLeft
      ls -> return ls
    let (pre, word, afterEq) = splitLongWord x
    when (pre /= pack "--") $ E.throw (SawNotLongArg x)
    when (word /= t) $ E.throw (SawWrongLongArg word)
    return s { remaining = xs
             , pendingLongArg = afterEq }

-- | Takes a single Text and returns a tuple, where the first element
-- is the first two letters, the second element is everything from the
-- third letter to the equal sign, and the third element is Nothing if
-- there is no equal sign, or Just Text with everything after the
-- equal sign if there is one.
splitLongWord :: Text -> (Text, Text, Maybe Text)
splitLongWord t = (f, s, r) where
  (f, rest) = X.splitAt 2 t
  (s, withEq) = X.break (== '=') rest
  r = case textHead withEq of
    Nothing -> Nothing
    (Just (_, afterEq)) -> Just afterEq

-- | Examines the next word. If it is a non-GNU long option, and it
-- matches a Text in the set unambiguously, returns a tuple of the
-- word actually found and the matching word in the set.
approxLongOpt :: (Error e) => Set LongOpt -> Parser e (Text, LongOpt)
approxLongOpt ts = Parser $ \s -> let
  err saw = ((Failed (unexpected (ExpApproxLong ts) saw)), s)
  good (found, match, st) = (Good (found, match), st)
  in E.switch err good $ do
    maybe (return ()) (E.throw . SawStillPendingShorts) (pendingShort s)
    maybe (return ()) (E.throw . SawPendingLong) (pendingLongArg s)
    (x:xs) <- case remaining s of
      [] -> E.throw SawNoArgsLeft
      r -> return r
    let (pre, word, afterEq) = splitLongWord x
    when (pre /= pack "--") (E.throw (SawNotLongArg x))
    let p (LongOpt t) = word `isPrefixOf` t
        matches = Set.filter p ts
    case Set.toList matches of
      [] -> E.throw (SawNoMatches word)
      (m:[]) -> let
        st' = s { remaining = xs
                , pendingLongArg = afterEq }
        in return (word, m, st')
      _ -> E.throw (SawMultipleMatches matches word)

tests :: [(String, IO ())]
tests = [ ("prop_noPendingShorts", quickCheck prop_noPendingShorts)
        , ("prop_WrongPendingShort", quickCheck prop_WrongPendingShort)
        , ("prop_goodPendingShort", quickCheck prop_goodPendingShort)
        ]

main :: IO ()
main = mapM_ (\(s, a) -> printf "%-25s: " s >> a) tests
