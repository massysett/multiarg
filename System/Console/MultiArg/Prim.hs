-- | Parser primitives. These are the only functions that have access
-- to the internals of the parser.
module System.Console.MultiArg.Prim (
    -- * Parser types
  Parser,
  ParserE,
  ParserSE,
  ParserT,
  
  -- * Running a parser
  
  -- | Each parser runner is applied to a list of Text, which are the
  -- command line arguments to parse. If there is any chance that you
  -- will be parsing Unicode strings, see the documentation in
  -- 'System.Console.MultiArg.GetArgs' before you use
  -- 'System.Environment.getArgs'.
  parse,
  parseE,
  parseSE,
  parseT,
  
  -- * Higher-level parser combinators
  parserMap,
  good,
  apply,
  choice,
  combine,
  lookAhead,
  
  -- ** Running parsers multiple times
  several,
  manyTill,

  -- ** Monad lifting
  parserLift,
  parserIO,

  -- ** Failure and errors
  throw,
  throwString,
  genericThrow,
  (<?>),
  try,
  
  -- * Parsers
  -- ** Short options and arguments
  pendingShortOpt,
  nonPendingShortOpt,
  pendingShortOptArg,  
  
  -- ** Long options and arguments
  exactLongOpt,
  approxLongOpt,

  -- ** Stoppers
  stopper,
  
  -- ** Positional (non-option) arguments
  nextArg,
  nonOptionPosArg,
  
  -- ** Miscellaneous
  end,
  
  -- * User state
  get,
  put,
  modify
  ) where


import qualified System.Console.MultiArg.Error as E
import System.Console.MultiArg.Option
  (ShortOpt,
    unShortOpt,
    LongOpt,
    unLongOpt )
import System.Console.MultiArg.TextNonEmpty
  ( TextNonEmpty ( TextNonEmpty ) )
import Control.Applicative ( Applicative, Alternative )
import qualified Control.Applicative
import Control.Monad.Exception.Synchronous
  (Exceptional(Success, Exception), switch )
import qualified Control.Monad.Exception.Synchronous as S
import Data.Functor.Identity ( runIdentity )
import Data.Text ( Text, pack, isPrefixOf, cons )
import qualified Data.Text as X
import qualified Data.Set as Set
import Data.Set ( Set )
import Control.Monad ( when, MonadPlus(mzero, mplus) )
import Control.Monad.Trans.Class ( lift )
import Data.Maybe ( isNothing )
import Data.Monoid ( Monoid ( mempty, mappend ) )
import Data.Functor.Identity ( Identity )
import Control.Monad.Trans.Class ( MonadTrans )
import Control.Monad.IO.Class ( MonadIO ( liftIO ) )

-- | Takes the head of a Text. Returns Nothing if the Text is empty.
textHead :: Text -> Maybe (Char, Text)
textHead t = case X.null t of
  True -> Nothing
  False -> Just (X.head t, X.tail t)

-- | Converts a Text to a TextNonEmpty. Returns Nothing if the Text is
-- empty.
toTextNonEmpty :: Text -> Maybe TextNonEmpty
toTextNonEmpty t = case textHead t of
  Nothing -> Nothing
  (Just (c, r)) -> Just $ TextNonEmpty c r

-- | Carries the internal state of the parser. The counter is a simple
-- way to determine whether the remaining list one ParseSt has been
-- modified from another. When parsers modify remaining, they
-- increment the counter.
data ParseSt s = ParseSt { pendingShort :: Maybe TextNonEmpty
                         , remaining :: [Text]
                         , sawStopper :: Bool
                         , userState :: s
                         , counter :: Int
                         } deriving (Show, Eq)

-- | Load up the ParseSt with an initial user state and a list of
-- commmand line arguments.
defaultState :: s -> [Text] -> ParseSt s
defaultState s ts = ParseSt { pendingShort = Nothing
                            , remaining = ts
                            , sawStopper = False
                            , userState = s
                            , counter = 0 }

-- | Carries the result of each parse.
data Result e a = Bad e | Good a

-- | @ParserT s e m a@ is a parser with user state s, error type e,
-- underlying monad m, and result type a. Internally the parser is a
-- state monad which keeps track of what is remaining to be
-- parsed. Since the parser has an internal state anyway, the user can
-- add to this state (this is called the user state.) The parser
-- ignores this user state so you can use it however you wish. If you
-- do not need a user state, just make it the unit type ().
--
-- The parser also includes the notion of failure. Any parser can
-- fail; a failed parser affects the behavior of combinators such as
-- combine. The failure type should be a instance of
-- System.Console.MultiArg.Error.Error. This allows you to define your
-- own type and use it for the failure type, which can be useful when
-- combining MultiArg with your own program.
--
-- The underlying monad is m. This makes ParserT into a monad
-- transformer; you can layer it on top of other monads. For instance
-- you might layer it on top of the IO monad so that your parser can
-- perform IO (for example, by examining the disk to see if arguments
-- that specify files are valid.) If you don't need a monad
-- transformer, just layer ParserT on top of Identity.
data ParserT s e m a =
  ParserT { runParserT :: ParseSt s -> m (Result e a, ParseSt s) }

instance (Monad m) => Functor (ParserT s e m) where
  fmap = parserMap

instance (Monad m) => Applicative (ParserT s e m) where
  pure = good
  (<*>) = apply

instance (Monad m, E.Error e) => Monoid (ParserT s e m a) where
  mempty = genericThrow
  mappend = choice

instance (Monad m, E.Error e) => Alternative (ParserT s e m) where
  empty = genericThrow
  (<|>) = choice
  many = several

instance (E.Error e, Monad m) => Monad (ParserT s e m) where
  (>>=) = combine
  return = good
  fail = throwString

instance (Monad m, E.Error e) => MonadPlus (ParserT s e m) where
  mzero = genericThrow
  mplus = choice

instance MonadTrans (ParserT s e) where
  lift = parserLift

instance (MonadIO m, E.Error e) => MonadIO (ParserT s e m) where
  liftIO = parserIO

-- | @ParserSE s e a@ is a parser with user state s, error type e,
-- underlying monad Identity, and result type a.
type ParserSE s e a = ParserT s e Identity a

-- | @ParserE e a@ is a parser with user state (), error type e,
-- underlying monad Identity, and result type a.
type ParserE e a = ParserT () e Identity a

-- | @Parser a@ is a parser with user state (), error type
-- SimpleError, underlying monad Identity, and result type a.
type Parser a = ParserT () E.SimpleError Identity a

-- | Runs a parser that has a user state and an underlying monad
-- Identity.
parseSE ::
  s
  -- ^ The initial user state
  
  -> [Text]
  -- ^ Command line arguments

  -> ParserSE s e a
  -- ^ Parser to run
  
  -> (Exceptional e a, s)
  -- ^ Success or failure, and the final user state
  
parseSE s ts p =
  let r = runIdentity (runParserT p (defaultState s ts))
      (result, st') = r
  in case result of
    (Good g) -> (Success g, userState st')
    (Bad e) -> (Exception e, userState st')

-- | Runs a parser that has no user state and an underlying monad of
-- Identity and is parameterized on the error type.
parseE ::
  [Text]
  -- ^ Command line arguments to parse
  
  -> ParserE e a
  -- ^ Parser to run
  
  -> Exceptional e a
  -- ^ Success or failure

parseE ts p =
  let r = runIdentity (runParserT p (defaultState () ts))
      (result, _) = r
  in case result of
    (Good g) -> Success g
    (Bad e) -> Exception e

-- | The simplest parser runner; has no user state, an underlying
-- monad Identity, and error type SimpleError.
parse :: [Text]
         -- ^ Command line arguments to parse
         
         -> Parser a
         -- ^ Parser to run

         -> Exceptional E.SimpleError a
         -- ^ Successful result or an error
parse = parseE

-- | The most complex parser runner. Runs a parser with a user-defined
-- state, error type, and underlying monad. Returns the final parse
-- result and the final user state, inside of the underlying monad.
parseT ::
  (Monad m)
  => s
  -- ^ Initial user state

  -> [Text]
  -- ^ Command line arguments to parse

  -> ParserT s e m a
  -- ^ Parser to run
  
  -> m (Exceptional e a, s)
  -- ^ Success or failure and the final user state, inside of the
  -- underlying monad

parseT s ts p = runParserT p (defaultState s ts) >>= \r ->
  let (result, st') = r
  in case result of
    (Good g) -> return (Success g, userState st')
    (Bad e) -> return (Exception e, userState st')

-- | Lifts a computation of the underlying monad into the ParserT
-- monad. This provides the implementation for
-- 'Control.Monad.Trans.Class.lift'.
parserLift ::
  Monad m
  => m a
  -> ParserT s e m a
parserLift c = ParserT $ \s ->
  c >>= \a -> return (Good a, s)

-- | Lifts a computation from the IO monad into the ParserT
-- monad. This provides the implementation for
-- 'Control.Monad.IO.Class.liftIO'.
parserIO ::
  (MonadIO m, E.Error e)
  => IO a
  -> ParserT s e m a
parserIO c = parserLift . liftIO $ c

-- | Combines two parsers into a single parser. The second parser can
-- optionally depend upon the result from the first parser.
--
-- This applies the first parser. If the first parser succeeds,
-- combine then takes the result from the first parser, applies the
-- function given to the result from the first parser, and then
-- applies the resulting parser.
--
-- If the first parser fails, combine will not apply the second
-- function but instead will bypass the second parser.
--
-- This provides the implementation for '>>=' in
-- 'Control.Monad.Monad'.
combine ::
  (Monad m)
  => ParserT s e m a
  -> (a -> ParserT s e m b)
  -> ParserT s e m b
combine (ParserT l) f = ParserT $ \s ->
  l s >>= \(r, s') ->
  case r of
    (Bad e) -> return (Bad e, s')
    (Good g) ->
      let (ParserT fr) = f g
      in fr s'

-- | @lookAhead p@ runs parser p. If p succeeds, lookAhead p succeeds
-- without consuming any input. If p fails without consuming any
-- input, so does lookAhead. If p fails and consumes input, lookAhead
-- also fails and consumes input. If this is undesirable, combine with
-- "try".
lookAhead ::
  (Monad m)
  => ParserT s e m a
  -> ParserT s e m a
lookAhead (ParserT p) = ParserT $ \s ->
  p s >>= \(r, s') ->
  return $ case r of
    (Good g) -> (Good g, s)
    (Bad e) -> (Bad e, s')

-- | @good a@ always succeeds without consuming any input and has
-- result a. This provides the implementation for
-- 'Control.Monad.Monad.return' and
-- 'Control.Applicative.Applicative.pure'.
good ::
  (Monad m)
  => a
  -> ParserT s e m a
good a = ParserT $ \s ->
  return (Good a, s)

-- | @throwString s@ always fails without consuming any input. The
-- failure contains a record of the string passed in by s. This
-- provides the implementation for 'Control.Monad.Monad.fail'.
throwString ::
  (E.Error e, Monad m)
  => String
  -> ParserT s e m a
throwString e = ParserT $ \s ->
  return (Bad (E.parseErr E.ExpOtherFailure
               (E.SawTextError (pack e))), s)

-- | @parserMap f p@ applies function f to the result of parser
-- p. First parser p is run. If it succeeds, function f is applied to
-- the result and another parser is returned with the result. If it
-- fails, f is not applied and a failed parser is returned. This
-- provides the implementation for 'Prelude.Functor.fmap'.
parserMap ::
  (Monad m)
  => (a -> b)
  -> ParserT s e m a
  -> ParserT s e m b
parserMap f (ParserT l) = ParserT $ \s ->
  l s >>= \r ->
  let (result, st') = r
  in case result of
    (Good g) -> return (Good (f g), st')
    (Bad e) -> return (Bad e, st')

-- | apply l r applies the function found in parser l to the result of
-- parser r. First the l parser is run. If it succeeds, it has a
-- resulting function. Then the r parser is run. If it succeeds, the
-- function from the l parser is applied to the result of the r
-- parser, and a new parser is returned with the result. If either
-- parser l or parser r fails, then a failed parser is returned. This
-- provides the implementation for '<*>' in
-- 'Control.Applicative.Applicative'.
apply ::
  (Monad m)
  => ParserT s e m (a -> b)
  -> ParserT s e m a
  -> ParserT s e m b
apply (ParserT x) (ParserT y) = ParserT $ \s ->
  x s >>= \r ->
  let (result, st') = r
  in case result of
    (Good f) -> y st' >>= \r' ->
      let (result', st'') = r'
      in case result' of
        (Good a) -> return (Good (f a), st'')
        (Bad e) -> return (Bad e, st'')
    (Bad e) -> return (Bad e, st')

-- | Fail with an unhelpful error message. Usually throw is more
-- useful, but this is handy to implement some typeclass instances.
genericThrow ::
  (Monad m, E.Error e)
  => ParserT s e m a
genericThrow = throw (E.parseErr E.ExpOtherFailure E.SawOtherFailure)

-- | throw e always fails without consuming any input and returns a
-- failed parser with error state e.
throw :: (Monad m) => e -> ParserT s e m a
throw e = ParserT $ \s ->
  return (Bad e, s)

noConsumed :: ParseSt s -> ParseSt s -> Bool
noConsumed old new = counter old >= counter new

-- | Runs the first parser. If it fails without consuming any input,
-- then runs the second parser. If the first parser succeeds, then
-- returns the result of the first parser. If the first parser fails
-- and consumes input, then returns the result of the first
-- parser. This provides the implementation for
-- '<|>' in 'Control.Applicative.Alternative'.
choice ::
  (Monad m)
  => ParserT s e m a
  -> ParserT s e m a
  -> ParserT s e m a
choice (ParserT l) (ParserT r) = ParserT $ \sOld ->
  l sOld >>= \(a, s') ->
  case a of
    (Good g) -> return (Good g, s')
    (Bad e) ->
      if noConsumed sOld s'
      then r sOld
      else return (Bad e, s')

-- | Runs the parser given. If it succeeds, then returns the result of
-- the parser. If it fails and consumes input, returns the result of
-- the parser. If it fails without consuming any input, then changes
-- the error using the function given.
(<?>) ::
  (Monad m)
  => ParserT s e m a
  -> e
  -> ParserT s e m a
(<?>) (ParserT l) e = ParserT $ \s ->
  l s >>= \(r, s') ->
  case r of
    (Good g) -> return (Good g, s')
    (Bad err) ->
      if noConsumed s s'
      then return (Bad e, s)
      else return (Bad err, s')

infix 0 <?>

increment :: ParseSt s -> ParseSt s
increment old = old { counter = succ . counter $ old }

-- | Parses only pending short options. Fails without consuming any
-- input if there has already been a stopper or if there are no
-- pending short options. Fails without consuming any input if there
-- is a pending short option, but it does not match the short option
-- given. Succeeds and consumes a pending short option if it matches
-- the short option given; returns the short option parsed.
pendingShortOpt ::
  (Monad m, E.Error e)
  => ShortOpt
  -> ParserT s e m ShortOpt
pendingShortOpt so = ParserT $ \s ->
  let err saw = return (Bad (E.parseErr (E.ExpPendingShortOpt so) saw), s)
      gd (res, newSt) = return (Good res, newSt)
  in switch err gd $ do
    when (sawStopper s) $ S.throw E.SawAlreadyStopper
    (TextNonEmpty first rest) <-
      maybe (S.throw E.SawNoPendingShorts) return (pendingShort s)
    when (unShortOpt so /= first)
      (S.throw $ E.SawWrongPendingShort first)
    return (so, increment s { pendingShort = toTextNonEmpty rest })

-- | Parses only non-pending short options. Fails without consuming
-- any input if, in order:
--
-- * there are pending short options
--
-- * there has already been a stopper
--
-- * there are no arguments left to parse
--
-- * the next argument is an empty string
--
-- * the next argument does not begin with a dash
--
-- * the next argument is a single dash
--
-- * the next argument is a short option but it does not match
--   the one given
--
-- * the next argument is a stopper
--
-- Otherwise, consumes the next argument, puts any remaining letters
-- from the argument into a pending short, and removes the first word
-- from remaining arguments to be parsed. Returns the short option
-- parsed.
nonPendingShortOpt ::
  (E.Error e, Monad m)
  => ShortOpt
  -> ParserT s e m ShortOpt
nonPendingShortOpt so = ParserT $ \s ->
  let err saw =
        return (Bad (E.parseErr (E.ExpNonPendingShortOpt so) saw), s)
      gd (g, n) = return (Good g, n)
  in switch err gd $ do
    maybe (return ()) (S.throw . E.SawStillPendingShorts) (pendingShort s)
    when (sawStopper s) (S.throw E.SawAlreadyStopper)
    (a:as) <- case remaining s of
      [] -> S.throw E.SawNoArgsLeft
      x -> return x
    (maybeDash, word) <- case textHead a of
      Nothing -> S.throw E.SawEmptyArg
      (Just w) -> return w
    when (maybeDash /= '-') $ S.throw (E.SawNotShortArg a)
    (letter, arg) <- case textHead word of
      Nothing -> S.throw E.SawSingleDashArg
      (Just w) -> return w
    when (letter /= unShortOpt so) $ S.throw (E.SawWrongShortArg letter)
    when (letter == '-') $ S.throw E.SawNewStopper
    let s' = increment s { pendingShort = toTextNonEmpty arg
                         , remaining = as }
    return (so, s')

-- | Parses an exact long option. That is, the text of the
-- command-line option must exactly match the text of the
-- option. Returns the option, and any argument that is attached to
-- the same word of the option with an equal sign (for example,
-- @--follow=\/dev\/random@ will return @Just \"\/dev\/random\"@ for the
-- argument.) If there is no equal sign, returns Nothing for the
-- argument. If there is an equal sign but there is nothing after it,
-- returns @Just \"\"@ for the argument.
--
-- If you do not want your long option to have equal signs and
-- GNU-style option arguments, wrap this parser in something that will
-- fail if there is an option argument.
--
-- Fails without consuming any input if:
--
-- * there are pending short options
--
-- * a stopper has been parsed
--
-- * there are no arguments left on the command line
--
-- * the next argument on the command line does not begin with
--   two dashes
--
-- * the next argument on the command line is @--@ (a stopper)
--
-- * the next argument on the command line does begin with two
--   dashes but its text does not match the argument we're looking for
exactLongOpt ::
  (E.Error e, Monad m)
  => LongOpt
  -> ParserT s e m (LongOpt, Maybe Text)
exactLongOpt lo = ParserT $ \s ->
  let err saw = return (Bad (E.parseErr (E.ExpExactLong lo) saw), s)
      gd (g, n) = return (Good g, n)
  in switch err gd $ do
    maybe (return ()) (S.throw . E.SawStillPendingShorts) (pendingShort s)
    when (sawStopper s) (S.throw E.SawAlreadyStopper)
    (x:xs) <- case remaining s of
      [] -> S.throw E.SawNoArgsLeft
      ls -> return ls
    let (pre, word, afterEq) = splitLongWord x
    when (pre /= pack "--") $ S.throw (E.SawNotLongArg x)
    when (X.null word && isNothing afterEq) (S.throw E.SawNewStopper)
    when (word /= unLongOpt lo) $ S.throw (E.SawWrongLongArg word)
    let s' = increment s { remaining = xs }
    return ((lo, afterEq), s')

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
approxLongOpt ::
  (E.Error e, Monad m)
  => Set LongOpt
  -> ParserT s e m (Text, LongOpt, Maybe Text)
approxLongOpt ts = ParserT $ \s ->
  let err saw = return (Bad (E.parseErr (E.ExpApproxLong ts) saw), s)
      gd (g, newSt) = return (Good g, newSt)
  in switch err gd $ do
    maybe (return ()) (S.throw . E.SawStillPendingShorts) (pendingShort s)
    (x:xs) <- case remaining s of
      [] -> S.throw E.SawNoArgsLeft
      r -> return r
    let (pre, word, afterEq) = splitLongWord x
    when (pre /= pack "--") (S.throw (E.SawNotLongArg x))
    when (X.null word && isNothing afterEq) (S.throw E.SawNewStopper)
    let p t = word `isPrefixOf` (unLongOpt t)
        matches = Set.filter p ts
    case Set.toList matches of
      [] -> S.throw (E.SawNoMatches word)
      (m:[]) -> let s' = increment s { remaining = xs }
                in return ((word, m, afterEq), s')
      _ -> S.throw (E.SawMultipleMatches matches word)

-- | Parses only pending short option arguments. For example, for the
-- @tail@ command, if you enter the option @-c25@, then after parsing
-- the @-c@ option the @25@ becomes a pending short option argument
-- because it was in the same command line argument as the @-c@.
--
-- Fails without consuming any input if:
--
-- * a stopper has already been parsed
--
-- * there are no pending short option arguments
--
-- On success, returns the text of the pending short option argument
-- (this text cannot be empty).
pendingShortOptArg ::
  (E.Error e, Monad m)
  => ParserT s e m Text
pendingShortOptArg = ParserT $ \s ->
  let err saw = return (Bad (E.parseErr E.ExpPendingShortArg saw), s)
      gd (g, newSt) = return (Good g, newSt)
  in switch err gd $ do
    when (sawStopper s) (S.throw E.SawAlreadyStopper)
    let f (TextNonEmpty c t) = return (c `cons` t)
    a <- maybe (S.throw E.SawNoPendingShortArg) f (pendingShort s)
    let newSt = increment s { pendingShort = Nothing }
    return (a, newSt)

-- | Parses a "stopper" - that is, a double dash. Changes the internal
-- state of the parser to reflect that a stopper has been seen.
stopper ::
  (E.Error e, Monad m)
  => ParserT s e m ()
stopper = ParserT $ \s ->
  let err saw = return (Bad (E.parseErr E.ExpStopper saw), s)
      gd (g, newSt) = return (Good g, newSt)
  in switch err gd $ do
    maybe (return ()) (S.throw . E.SawStillPendingShorts) (pendingShort s)
    when (sawStopper s) $ S.throw E.SawAlreadyStopper
    (x:xs) <- case remaining s of
      [] -> S.throw E.SawNoArgsLeft
      r -> return r
    when (not $ pack "--" `isPrefixOf` x) (S.throw E.SawNotStopper)
    when (X.length x /= 2) (S.throw E.SawNotStopper)
    let s' = increment s { sawStopper = True
                         , remaining = xs }
    return ((), s')

-- | try p behaves just like p, but if p fails, try p will not consume
-- any input.
try :: Monad m => ParserT s e m a -> ParserT s e m a
try (ParserT l) = ParserT $ \s ->
  l s >>= \(r, s') ->
  case r of
    (Good g) -> return (Good g, s')
    (Bad e) -> return (Bad e, s)

-- | Returns the next string on the command line as long as there are
-- no pendings. Be careful - this will return the next string even if
-- it looks like an option (that is, it starts with a dash.) Consider
-- whether you should be using nonOptionPosArg instead. However this
-- can be useful when parsing command line options after a stopper.
nextArg ::
  (E.Error e, Monad m)
  => ParserT s e m Text
nextArg = ParserT $ \s ->
  let err saw = return (Bad (E.parseErr E.ExpNextArg saw), s)
      gd (g, newSt) = return (Good g, newSt)
  in switch err gd $ do
    maybe (return ()) (S.throw . E.SawStillPendingShorts) (pendingShort s)
    (x:xs) <- case remaining s of
      [] -> S.throw E.SawNoArgsLeft
      r -> return r
    let newSt = increment s { remaining = xs }
    return (x, newSt)

-- | Returns the next string on the command line as long as there are
-- no pendings and as long as the next string does not begin with a
-- dash. If there has already been a stopper, then will return the
-- next string even if it starts with a dash.
nonOptionPosArg ::
  (E.Error e, Monad m)
  => ParserT s e m Text
nonOptionPosArg = ParserT $ \s ->
  let err saw = return (Bad (E.parseErr E.ExpNonOptionPosArg saw), s)
      gd (g, newSt) = return (Good g, newSt)
  in switch err gd $ do
    maybe (return ()) (S.throw . E.SawStillPendingShorts) (pendingShort s)
    (x:xs) <- case remaining s of
      [] -> S.throw E.SawNoArgsLeft
      r -> return r
    result <- if sawStopper s
              then return x
              else case textHead x of
                Just ('-', _) -> S.throw $ E.SawLeadingDashArg x
                _ -> return x
    let newSt = increment s { remaining = xs }
    return (result, newSt)

-- | manyTill p e runs parser p repeatedly until parser e succeeds.
--
-- More precisely, first it runs parser e. If parser e succeeds, then
-- manyTill returns the result of all the preceding successful parses
-- of p. If parser e fails (it does not matter whether e consumed any
-- input or not), manyTill runs parser p again. What happens next
-- depends on whether p succeeded or failed. If p succeeded, then the
-- loop starts over by running parser e again. If p failed (it does
-- not matter whether it consumed any input or not), then manyTill
-- fails. The state of the parser is updated to reflect its state
-- after the failed run of p, and the parser is left in a failed
-- state.
--
-- Should parser e succeed (as it will on a successful application of
-- manyTill), then the parser state will reflect that parser e
-- succeeded--that is, if parser e consumes input, that input will be
-- consumed in the parser that is returned. Wrap e inside of
-- @lookAhead@ if that is undesirable.
--
-- Be particularly careful to get the order of the arguments
-- correct. Applying this function to reversed arguments will yield
-- bugs that are very difficult to diagnose.
manyTill ::
  Monad m
  => ParserT s e m a
  -> ParserT s e m end
  -> ParserT s e m [a]
manyTill (ParserT r) (ParserT f) = ParserT $ \s ->
  parseTill s r f >>= \till ->
  case lastFailure till of
    Nothing -> return (Good (goods till), lastSt till)
    (Just e) -> return (Bad e, lastSt till)

data Till s e a =
  Till { goods :: [a]
       , lastSt :: ParseSt s
       , lastFailure :: Maybe e }

parseTill ::
  (Monad m)
  => ParseSt s
  -> (ParseSt s -> m (Result e a, ParseSt s))
  -> (ParseSt s -> m (Result e b, ParseSt s))
  -> m (Till s e a)
parseTill s fr ff = ff s >>= \r ->
  case r of
    (Good _, s') -> return $ Till [] s' Nothing
    (Bad _, _) ->
      fr s >>= \r' ->
      case r' of
        (Bad e, s') -> return $ Till [] s' (Just e)
        (Good g, s') ->
          parseTill s' fr ff >>= \r'' ->
          let (Till gs lS lF) = r''
          in if counter s' == counter s
             then error "parseTill applied to parser that takes empty list"
             else return $ Till (g:gs) lS lF

-- | several p runs parser p zero or more times and returns all the
-- results. This proceeds like this: parser p is run and, if it
-- succeeds, the result is saved and parser p is run
-- again. Repeat. Eventually this will have to fail. If the last run
-- of parser p fails without consuming any input, then several p runs
-- successfully. The state of the parser is updated to reflect the
-- successful runs of p. If the last run of parser p fails but it
-- consumed input, then several p fails. The state of the parser is
-- updated to reflect the state up to and including the run that
-- partially consumed input. The parser is left in a failed state.
--
-- This semantic can come in handy. For example you might run a parser
-- multiple times that parses an option and arguments to the
-- option. If the arguments fail to parse, then several will fail.
several ::
  (Monad m)
  => ParserT s e m a
  -> ParserT s e m [a]
several (ParserT l) = ParserT $ \s ->
  parseRepeat s l >>= \r ->
  let (result, finalGoodSt, failure, finalBadSt) = r
  in if noConsumed finalGoodSt finalBadSt
     then return (Good result, finalGoodSt)
     else return (Bad failure, finalBadSt)

parseRepeat ::
  (Monad m)
  => ParseSt s
  -> (ParseSt s -> m (Result e a, ParseSt s))
  -> m ([a], ParseSt s, e, ParseSt s)
parseRepeat st1 f = f st1 >>= \r ->
  case r of
    (Good a, st') ->
      if noConsumed st1 st'
      then error "parseRepeat applied to parser that takes empty list"
      else parseRepeat st' f >>= \r' ->
      let (ls, finalGoodSt, failure, finalBadSt) = r'
      in return (a : ls, finalGoodSt, failure, finalBadSt)
    (Bad e, st') -> return ([], st1, e, st')

-- | Succeeds if there is no more input left.
end ::
  (E.Error e, Monad m)
  => ParserT s e m ()
end = ParserT $ \s ->
  let err saw = return (Bad (E.parseErr E.ExpEnd saw), s)
      gd (g, newSt) = return (Good g, newSt)
  in switch err gd $ do
    maybe (return ()) (S.throw . E.SawStillPendingShorts) (pendingShort s)
    when (not . null . remaining $ s) (S.throw E.SawMoreInput)
    return ((), s)

-- | Gets the user state.
get ::
  (Monad m)
  => ParserT s e m s
get = ParserT $ \s ->
  return (Good (userState s), s)

-- | Puts a new user state.
put ::
  (Monad m)
  => s
  -> ParserT s e m ()
put newUserSt = ParserT $ \s ->
  return (Good (), s { userState = newUserSt })

-- | Modify the user state.
modify ::
  (Monad m)
  => (s -> s)
  -> ParserT s e m ()
modify f = ParserT $ \s ->
  return (Good (), s { userState = f (userState s) })
