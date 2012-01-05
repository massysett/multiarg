{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Console.MultiArg.Prim where

import qualified System.Console.MultiArg.Error as E
import System.Console.MultiArg.Error ( unexpected )
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
                         , counter :: Int
                         } deriving (Show, Eq)

defaultState :: s -> [Text] -> ParseSt s
defaultState user ts = ParseSt { pendingShort = Nothing
                               , remaining = ts
                               , sawStopper = False
                               , userState = user
                               , counter = 0 }

newtype ParserSE s e a =
  ParserSE { unParserSE :: ExceptionalT e (State (ParseSt s)) a }
  deriving (Monad, Functor, Applicative)

type ParserE e a = ParserSE ()
type Parser a = ParserSE () E.SimpleError a

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

-- | Runs the first parser. If it fails without consuming any input,
-- then runs the second parser. If the first parser succeeds, then
-- returns the result of the first parser. If the first parser fails
-- and consumes input, then returns the result of the first parser.
(<|>) :: ParserSE s e a -> ParserSE s e a -> ParserSE s e a
(<|>) (ParserSE l) (ParserSE r) = ParserSE $ do
  s <- lift get
  let (a1, s') = flip runState s . runExceptionalT $ l
  case a1 of
    (Success g) -> l
    (Exception f) ->
      if noConsumed s s' then r else l

infixr 1 <|>

noConsumed :: ParseSt st -> ParseSt st -> Bool
noConsumed l r = counter l == counter r

-- | Runs the parser given. If it succeeds, then returns the result of
-- the parser. If it fails and consumes input, returns the result of
-- the parser. If it fails without consuming any input, then changes
-- the error using the function given.
(<?>) :: ParserSE s e a -> (e -> e) -> ParserSE s e a
(<?>) (ParserSE l) f = ParserSE $ do
  s <- lift get
  let (a1, s') = flip runState s . runExceptionalT $ l
  case a1 of
    (Success g) -> l
    (Exception e') ->
      if noConsumed s s' then throwT (f e') else l

infix 0 <?>

-- | Increments the count of how many times the state has been
-- modified.
increment :: ExceptionalT e (State (ParseSt s)) ()
increment = lift $ modify (\s -> s { counter = succ . counter $ s } )

-- | Parses only pending short options. Fails without consuming any
-- input if there has already been a stopper or if there are no
-- pending short options. Fails without consuming any input if there
-- is a pending short option, but it does not match the short option
-- given. Succeeds and consumes a pending short option if it matches
-- the short option given; returns the short option parsed.
pendingShortOpt :: (E.Error e) => ShortOpt -> ParserSE s e ShortOpt
pendingShortOpt so = ParserSE $ do
  s <- lift get
  let err saw = throwT (unexpected (E.ExpCharOpt so) saw)
  when (sawStopper s) (err E.SawAlreadyStopper)
  (TextNonEmpty first rest) <-
    maybe (err E.SawNoPendingShorts) return (pendingShort s)
  when (unShortOpt so /= first) (err $ E.SawWrongPendingShort first)
  lift $ put s { pendingShort = toTextNonEmpty rest }
  increment
  return so

-- | Parses only non-pending short options. Fails without consuming
-- any input if, in order:
--
-- * there are pending short options
-- * there has already been a stopper
-- * there are no arguments left to parse
-- * the next argument is an empty string
-- * the next argument does not begin with a dash
-- * the next argument is a single dash
-- * the next argument is a short option but it does not match the one given
-- * the next argument is a stopper
--
-- Otherwise, consumes the next argument, puts any remaining letters
-- from the argument into a pending short, and removes the first word
-- from remaining arguments to be parsed. Returns the short option
-- parsed.
nonPendingShortOpt :: (E.Error e) => ShortOpt -> ParserSE s e ShortOpt
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
  increment
  return so

exactLongOpt :: (E.Error e)
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
  increment
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
approxLongOpt :: (E.Error e)
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
      increment
      return (word, m, afterEq)
    _ -> err (E.SawMultipleMatches matches word)

pendingShortOptArg :: (E.Error e) => ParserSE s e Text
pendingShortOptArg = ParserSE $ do
  let err saw = throwT $ unexpected E.ExpPendingShortArg saw
  s <- lift get
  when (sawStopper s) (err E.SawAlreadyStopper)
  let f (TextNonEmpty c t) = return (c `cons` t)
  a <- maybe (err E.SawNoPendingShortArg) f (pendingShort s)
  lift $ put s { pendingShort = Nothing }
  increment
  return a

stopper :: (E.Error e) => ParserSE s e ()
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
  increment
  lift $ put s { sawStopper = True
               , remaining = xs }

require :: ParserSE s e a -> ParserSE s e a
require (ParserSE l) = ParserSE $ do
  s <- lift get
  let (e, s') = flip runState s . runExceptionalT $ l
  case e of
    (Success _) -> l
    (Exception err) ->
      if noConsumed s s'
      then increment >> l
      else l

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
nextArg :: (E.Error e) => ParserSE s e Text
nextArg = ParserSE $ do
  let err saw = throwT $ unexpected E.ExpNextArg saw
  s <- lift get
  maybe (return ()) (err . E.SawStillPendingShorts) (pendingShort s)
  (x:xs) <- case remaining s of
    [] -> err E.SawNoArgsLeft
    r -> return r
  lift $ put s { remaining = xs }
  increment
  return x

-- | Returns the next string on the command line as long as there are
-- no pendings and as long as the next string does not begin with a
-- dash. If there has already been a stopper, then will return the
-- next string even if it starts with a dash.
nonOptionPosArg :: (E.Error e) => ParserSE s e Text
nonOptionPosArg = ParserSE $ do
  let err saw = throwT $ unexpected E.ExpNonOptionPosArg saw
  s <- lift get
  maybe (return ()) (err . E.SawStillPendingShorts) (pendingShort s)
  (x:xs) <- case remaining s of
    [] -> err E.SawNoArgsLeft
    r -> return r
  result <- if sawStopper s
            then return x
            else case textHead x of
              Just ('-', _) -> err $ E.SawLeadingDashArg x              
              _ -> return x
  lift $ put s { remaining = xs }
  increment
  return result

parseRepeat :: ParseSt s
          -> (ParseSt s -> (Exceptional e a, ParseSt s))
          -> ([a], ParseSt s, e, ParseSt s)
parseRepeat st1 f = case f st1 of
  (Success a, st') -> let
    (ls, finalGoodSt, failure, finalBadSt) = parseRepeat st' f
    in (a : ls, finalGoodSt, failure, finalBadSt)
  (Exception e, st') -> ([], st1, e, st')

many :: ParserSE s e a -> ParserSE s e [a]
many (ParserSE l) = ParserSE $ do
  s <- lift get
  let f st = runIdentity
             . flip runStateT st
             . runExceptionalT
             $ l
      (result, finalGoodSt, failure, finalBadSt) = parseRepeat s f
  if noConsumed finalGoodSt finalBadSt
    then (lift . put $ finalGoodSt) >> return result
    else (lift . put $ finalBadSt) >> throwT failure

-- | Succeeds if there is no more input left.
end :: (E.Error e) => ParserSE s e ()
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

