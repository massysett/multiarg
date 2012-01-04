{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Console.MultiArg.MultiArg where

import qualified Control.Monad.Exception.Synchronous as E
import Control.Applicative ( Applicative )
import Control.Monad.Exception.Synchronous
  ( ExceptionalT, runExceptionalT, throwT, Exceptional(Success, Exception) )
import Control.Monad.Trans.State.Lazy ( State, get, runState, put,
                                        modify, runStateT )
import Data.Functor.Identity ( runIdentity )
import qualified Control.Monad.Trans.State.Lazy as St
import Data.Text ( Text, pack, unpack, isPrefixOf, cons )
import qualified Data.Text as X
import qualified Data.Set as Set
import Data.Set ( Set )
import Control.Monad ( when, liftM )
import Control.Monad.Trans.Class ( lift )
import Test.QuickCheck ( Arbitrary ( arbitrary ),
                         suchThat )
import Text.Printf ( printf )
import Data.Maybe ( isNothing, fromMaybe )

data Expecting = ExpCharOpt ShortOpt
                 | ExpExactLong LongOpt
                 | ExpApproxLong (Set LongOpt)
                 | ExpPendingLongOptArg
                 | ExpPendingShortArg
                 | ExpStopper
                 | ExpNextArg
                 | ExpNonOptionPosArg
                 | ExpEnd
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
         | SawLeadingDashArg Text
         | SawMoreInput
         deriving (Show, Eq)
           
newtype ShortOpt = ShortOpt Char deriving (Show, Eq)
instance Arbitrary ShortOpt where
  arbitrary = do
    c <- suchThat arbitrary (/= '-')
    return $ ShortOpt c

makeShortOpt :: Char -> ShortOpt
makeShortOpt c = case c of
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

class Error e where
  unexpected :: Expecting -> Saw -> e
  changeExpecting :: Expecting -> e -> e

data SimpleError = SimpleError Expecting Saw deriving (Show, Eq)
instance Error SimpleError where
  unexpected = SimpleError
  changeExpecting exp' (SimpleError exp s) = SimpleError exp' s

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

infixr 1 <|>

noConsumed :: ParseSt st -> ParseSt st -> Bool
noConsumed = undefined

(<?>) :: ParserSE s e a -> (e -> e) -> ParserSE s e a
(<?>) (ParserSE l) f = ParserSE $ do
  s <- lift get
  let (a1, s') = flip runState s . runExceptionalT $ l
  case a1 of
    (Success g) -> lift (put s') >> return g
    (Exception e') ->
      if noConsumed s s' then throwT (f e')
      else lift (put s') >> throwT e'

infix 0 <?>

pendingShortOpt :: (Error e) => ShortOpt -> ParserSE s e ShortOpt
pendingShortOpt so@(ShortOpt c) = ParserSE $ do
  s <- lift get
  let err saw = throwT (unexpected (ExpCharOpt so) saw)
  when (sawStopper s) (err SawAlreadyStopper)
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

exactLongOpt :: (Error e)
                => LongOpt
                -> ParserSE s e (LongOpt, Maybe Text)
exactLongOpt lo@(LongOpt t) = ParserSE $ do
  let err saw = throwT (unexpected (ExpExactLong lo) saw)
  s <- lift get
  maybe (return ()) (err . SawStillPendingShorts) (pendingShort s)
  when (sawStopper s) (err SawAlreadyStopper)
  (x:xs) <- case remaining s of
    [] -> err SawNoArgsLeft
    ls -> return ls
  let (pre, word, afterEq) = splitLongWord x
  when (pre /= pack "--") $ err (SawNotLongArg x)
  when (X.null word && isNothing afterEq) (err SawNewStopper)
  when (word /= t) $ err (SawWrongLongArg word)
  lift $ put s { remaining = xs }
  return (lo, afterEq)

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

-- | Examines the next word. If it matches a Text in the set
-- unambiguously, returns a tuple of the word actually found and the
-- matching word in the set.
approxLongOpt :: (Error e)
                 => Set LongOpt
                 -> ParserSE s e (Text, LongOpt, Maybe Text)
approxLongOpt ts = ParserSE $ do
  s <- lift get
  let err saw = throwT $ unexpected (ExpApproxLong ts) saw
  maybe (return ()) (err . SawStillPendingShorts) (pendingShort s)
  (x:xs) <- case remaining s of
    [] -> err SawNoArgsLeft
    r -> return r
  let (pre, word, afterEq) = splitLongWord x
  when (pre /= pack "--") (err (SawNotLongArg x))
  when (X.null word && isNothing afterEq) (err SawNewStopper)
  let p (LongOpt t) = word `isPrefixOf` t
      matches = Set.filter p ts
  case Set.toList matches of
    [] -> err (SawNoMatches word)
    (m:[]) -> do
      lift $ put s { remaining = xs }
      return (word, m, afterEq)
    _ -> err (SawMultipleMatches matches word)

pendingShortOptArg :: (Error e) => ParserSE s e Text
pendingShortOptArg = ParserSE $ do
  let err saw = throwT $ unexpected ExpPendingShortArg saw
  s <- lift get
  when (sawStopper s) (err SawAlreadyStopper)
  let f (TextNonEmpty c t) = return (c `cons` t)
  a <- maybe (err SawNoPendingShortArg) f (pendingShort s)
  lift $ put s { pendingShort = Nothing }
  return a

stopper :: (Error e) => ParserSE s e ()
stopper = ParserSE $ do
  let err saw = throwT $ unexpected ExpStopper saw
  s <- lift get
  maybe (return ()) (err . SawStillPendingShorts) (pendingShort s)
  when (sawStopper s) $ err SawAlreadyStopper
  (x:xs) <- case remaining s of
    [] -> err SawNoArgsLeft
    r -> return r
  when (not $ pack "--" `isPrefixOf` x) (err SawNotStopper)
  when (X.length x /= 2) (err SawNotStopper)
  lift $ put s { sawStopper = True
               , remaining = xs }

try :: ParserSE s e a -> ParserSE s e a
try (ParserSE l) = ParserSE $ do
  s <- lift get
  let (e, s') = flip runState s . runExceptionalT $ l
  case e of
    (Success g) -> lift (put s') >> return g
    (Exception err) -> throwT err

-- | Returns the next string on the command line as long as there are
-- no pendings. Be careful - this will return the next string even if
-- it looks like an option (that is, it starts with a dash.) Consider
-- whether you should be using nonOptionPosArg instead. However this
-- can be useful when parsing command line options after a stopper.
nextArg :: (Error e) => ParserSE s e Text
nextArg = ParserSE $ do
  let err saw = throwT $ unexpected ExpNextArg saw
  s <- lift get
  maybe (return ()) (err . SawStillPendingShorts) (pendingShort s)
  (x:xs) <- case remaining s of
    [] -> err SawNoArgsLeft
    r -> return r
  lift $ put s { remaining = xs }
  return x

-- | Returns the next string on the command line as long as there are
-- no pendings and as long as the next string does not begin with a
-- dash.
nonOptionPosArg :: (Error e) => ParserSE s e Text
nonOptionPosArg = ParserSE $ do
  let err saw = throwT $ unexpected ExpNonOptionPosArg saw
  s <- lift get
  maybe (return ()) (err . SawStillPendingShorts) (pendingShort s)
  (x:xs) <- case remaining s of
    [] -> err SawNoArgsLeft
    r -> return r
  case textHead x of
    Just ('-', _) -> err $ SawLeadingDashArg x
    Nothing -> return ()
    _ -> return ()
  lift $ put s { remaining = xs }
  return x

parseRepeat :: ParseSt s
          -> (ParseSt s -> (Exceptional e a, ParseSt s))
          -> ([a], ParseSt s)
parseRepeat st1 f = case f st1 of
  (Success a, st') -> let
    (ls, finalSt) = parseRepeat st' f
    in (a : ls, finalSt)
  (Exception _, _) -> ([], st1)

many :: ParserSE s e a -> ParserSE s e [a]
many (ParserSE l) = ParserSE $ do
  s <- lift get
  let f st = runIdentity
             . flip runStateT st
             . runExceptionalT
             $ l
      (result, finalSt) = parseRepeat s f
  lift $ put finalSt
  return result

-- | Succeeds if there is no more input left.
end :: (Error e) => ParserSE s e ()
end = ParserSE $ do
  let err saw = throwT $ unexpected ExpEnd saw
  s <- lift get
  maybe (return ()) (err . SawStillPendingShorts) (pendingShort s)
  when (not . null . remaining $ s) (err SawMoreInput)
  return ()

-- | Gets the user state.
getSt :: ParserSE s e s
getSt = ParserSE $ do
  s <- lift get
  return $ userState s

-- | Changes the user state.
putSt :: s -> ParserSE s e ()
putSt s = ParserSE $ do
  lift $ modify (\st -> st { userState = s } )

-- | Modify the user state.
modifySt :: (s -> s) -> ParserSE s e ()
modifySt f = ParserSE $ do
  s <- lift get
  let u = f (userState s)
  lift $ put s { userState = u }

------------------------------------------------------------
------------------------------------------------------------


option :: a -> ParserSE s e a -> ParserSE s e a
option x p = p <|> return x

optionMaybe :: ParserSE s e a -> ParserSE s e (Maybe a)
optionMaybe p = option Nothing (liftM Just p)

shortOpt :: (Error e)
            => ShortOpt
            -> ParserSE s e ShortOpt
shortOpt s = pendingShortOpt s <|> nonPendingShortOpt s

shortOptional :: (Error e)
                 => ShortOpt
                 -> ParserSE s e (ShortOpt, Maybe Text)
shortOptional s = do
  so <- shortOpt s
  a <- optionMaybe (pendingShortOptArg <|> nonOptionPosArg)
  return (so, a)

shortSingle :: (Error e) =>
               ShortOpt
               -> ParserSE s e (ShortOpt, Text)
shortSingle s = do
  so <- shortOpt s
  a <- pendingShortOptArg <|> nextArg
  return (so, a)

shortDouble :: (Error e)
               => ShortOpt
               -> ParserSE s e (ShortOpt, Text, Text)
shortDouble s = do
  (so, a1) <- shortSingle s
  a2 <- nextArg
  return (so, a1, a2)

shortVariable :: (Error e)
                 => ShortOpt
                 -> ParserSE s e (ShortOpt, [Text])
shortVariable s = do
  so <- shortOpt s
  firstArg <- optionMaybe pendingShortOptArg
  rest <- many nonOptionPosArg
  let result = maybe rest ( : rest ) firstArg
  return (so, result)

