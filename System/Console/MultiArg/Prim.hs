{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Console.MultiArg.Prim where

import qualified System.Console.MultiArg.Error as E
import System.Console.MultiArg.Option
  ( TextNonEmpty ( TextNonEmpty ),
    ShortOpt,
    unShortOpt,
    LongOpt,
    unLongOpt )
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

class Error e where
  unexpected :: E.Expecting -> E.Saw -> e
  changeExpecting :: E.Expecting -> e -> e

data SimpleError = SimpleError E.Expecting E.Saw deriving (Show, Eq)
instance Error SimpleError where
  unexpected = SimpleError
  changeExpecting exp' (SimpleError exp s) = SimpleError exp' s

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

defaultState :: s -> [Text] -> ParseSt s
defaultState user ts = ParseSt { pendingShort = Nothing
                               , remaining = ts
                               , sawStopper = False
                               , userState = user }

newtype ParserSE s e a =
  ParserSE { unParserSE :: ExceptionalT e (State (ParseSt s)) a }
  deriving (Monad, Functor, Applicative)

type ParserE e a = ParserSE ()
type Parser a = ParserSE () SimpleError a

runParserSE :: s
               -> [Text]
               -> ParserSE s e a
               -> Exceptional e (a, s)
runParserSE user ts (ParserSE c) = let
  s = defaultState user ts
  in case flip runState s . runExceptionalT $ c of
    ((Success g), s') -> Success (g, userState s')
    ((Exception e), _) -> Exception e

runParser :: [Text]
             -> ParserSE () e a
             -> Exceptional e a
runParser ts c = case runParserSE () ts c of
  (Success (g, _)) -> Success g
  (Exception e) -> Exception e

-- | Always fails without consuming any input.
zero :: e -> ParserSE s e a
zero e = ParserSE $ throwT e

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
pendingShortOpt so = ParserSE $ do
  s <- lift get
  let err saw = throwT (unexpected (E.ExpCharOpt so) saw)
  when (sawStopper s) (err E.SawAlreadyStopper)
  (TextNonEmpty first rest) <-
    maybe (err E.SawNoPendingShorts) return (pendingShort s)
  when (unShortOpt so /= first) (err $ E.SawWrongPendingShort first)
  lift $ put s { pendingShort = toTextNonEmpty rest } 
  return so

nonPendingShortOpt :: (Error e) => ShortOpt -> ParserSE s e ShortOpt
nonPendingShortOpt so = ParserSE $ do
  let err saw = throwT (unexpected (E.ExpCharOpt so) saw)
  s <- lift get
  maybe (return ()) (err . E.SawStillPendingShorts) (pendingShort s)
  when (sawStopper s) (err E.SawAlreadyStopper)
  (a:as) <- case remaining s of
    [] -> err E.SawNoArgsLeft
    x -> return x
  (maybeDash, word) <- case textHead a of
    Nothing -> err E.SawEmptyArg
    (Just w) -> return w
  when (maybeDash /= '-') $ err (E.SawNotShortArg a)
  (letter, arg) <- case textHead word of
    Nothing -> err E.SawSingleDashArg
    (Just w) -> return w
  when (letter /= unShortOpt so) $ err (E.SawWrongShortArg letter)
  when (letter == '-') $ err E.SawNewStopper
  lift $ put s { pendingShort = toTextNonEmpty arg
               , remaining = as }
  return so

exactLongOpt :: (Error e)
                => LongOpt
                -> ParserSE s e (LongOpt, Maybe Text)
exactLongOpt lo = ParserSE $ do
  let err saw = throwT (unexpected (E.ExpExactLong lo) saw)
  s <- lift get
  maybe (return ()) (err . E.SawStillPendingShorts) (pendingShort s)
  when (sawStopper s) (err E.SawAlreadyStopper)
  (x:xs) <- case remaining s of
    [] -> err E.SawNoArgsLeft
    ls -> return ls
  let (pre, word, afterEq) = splitLongWord x
  when (pre /= pack "--") $ err (E.SawNotLongArg x)
  when (X.null word && isNothing afterEq) (err E.SawNewStopper)
  when (word /= unLongOpt lo) $ err (E.SawWrongLongArg word)
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
  let err saw = throwT $ unexpected (E.ExpApproxLong ts) saw
  maybe (return ()) (err . E.SawStillPendingShorts) (pendingShort s)
  (x:xs) <- case remaining s of
    [] -> err E.SawNoArgsLeft
    r -> return r
  let (pre, word, afterEq) = splitLongWord x
  when (pre /= pack "--") (err (E.SawNotLongArg x))
  when (X.null word && isNothing afterEq) (err E.SawNewStopper)
  let p t = word `isPrefixOf` (unLongOpt t)
      matches = Set.filter p ts
  case Set.toList matches of
    [] -> err (E.SawNoMatches word)
    (m:[]) -> do
      lift $ put s { remaining = xs }
      return (word, m, afterEq)
    _ -> err (E.SawMultipleMatches matches word)

pendingShortOptArg :: (Error e) => ParserSE s e Text
pendingShortOptArg = ParserSE $ do
  let err saw = throwT $ unexpected E.ExpPendingShortArg saw
  s <- lift get
  when (sawStopper s) (err E.SawAlreadyStopper)
  let f (TextNonEmpty c t) = return (c `cons` t)
  a <- maybe (err E.SawNoPendingShortArg) f (pendingShort s)
  lift $ put s { pendingShort = Nothing }
  return a

stopper :: (Error e) => ParserSE s e ()
stopper = ParserSE $ do
  let err saw = throwT $ unexpected E.ExpStopper saw
  s <- lift get
  maybe (return ()) (err . E.SawStillPendingShorts) (pendingShort s)
  when (sawStopper s) $ err E.SawAlreadyStopper
  (x:xs) <- case remaining s of
    [] -> err E.SawNoArgsLeft
    r -> return r
  when (not $ pack "--" `isPrefixOf` x) (err E.SawNotStopper)
  when (X.length x /= 2) (err E.SawNotStopper)
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
  let err saw = throwT $ unexpected E.ExpNextArg saw
  s <- lift get
  maybe (return ()) (err . E.SawStillPendingShorts) (pendingShort s)
  (x:xs) <- case remaining s of
    [] -> err E.SawNoArgsLeft
    r -> return r
  lift $ put s { remaining = xs }
  return x

-- | Returns the next string on the command line as long as there are
-- no pendings and as long as the next string does not begin with a
-- dash.
nonOptionPosArg :: (Error e) => ParserSE s e Text
nonOptionPosArg = ParserSE $ do
  let err saw = throwT $ unexpected E.ExpNonOptionPosArg saw
  s <- lift get
  maybe (return ()) (err . E.SawStillPendingShorts) (pendingShort s)
  (x:xs) <- case remaining s of
    [] -> err E.SawNoArgsLeft
    r -> return r
  case textHead x of
    Just ('-', _) -> err $ E.SawLeadingDashArg x
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
  let err saw = throwT $ unexpected E.ExpEnd saw
  s <- lift get
  maybe (return ()) (err . E.SawStillPendingShorts) (pendingShort s)
  when (not . null . remaining $ s) (err E.SawMoreInput)
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

choice :: e -> [ParserSE s e a] -> ParserSE s e a
choice e ls = foldl (<|>) (zero e) ls

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

nonGNUexactLongOpt :: (Error e)
                      => LongOpt
                      -> ParserSE s e LongOpt
nonGNUexactLongOpt l = try $ do
  (lo, maybeArg) <- exactLongOpt l
  case maybeArg of
    Nothing -> return lo
    (Just t) ->
      zero (unexpected (E.ExpNonGNUExactLong l)
            (E.SawGNULongOptArg t))

matchApproxLongOpt :: (Error e)
                      => LongOpt
                      -> Set LongOpt
                      -> ParserSE s e (Text, LongOpt, Maybe Text)
matchApproxLongOpt l s = try $ do
  a@(t, lo, _) <- approxLongOpt s
  if lo == l
    then return a
    else zero (unexpected (E.ExpMatchingApproxLong l s)
               (E.SawNotMatchingApproxLong t lo))

matchNonGNUApproxLongOpt :: (Error e)
                            => LongOpt
                            -> Set LongOpt
                            -> ParserSE s e (Text, LongOpt)
matchNonGNUApproxLongOpt l s = try $ do
  a@(t, lo, arg) <- matchApproxLongOpt l s
  let err b = zero (unexpected (E.ExpNonGNUMatchingApproxLong l s)
                    (E.SawMatchingApproxLongWithArg b))
  maybe (return (t, lo)) err arg

-- | Examines the possible words in Set. If there are no pendings,
-- then get the next word and see if it matches one of the words in
-- Set. If so, returns the word actually parsed and the matching word
-- from Set. If there is no match, fails without consuming any input.
