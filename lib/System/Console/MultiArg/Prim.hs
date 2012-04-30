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
  feed,

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
    makeShortOpt,
    LongOpt,
    unLongOpt, 
    makeLongOpt )
import System.Console.MultiArg.TextNonEmpty
  ( TextNonEmpty ( TextNonEmpty ) )
import Control.Applicative ( Applicative, Alternative )
import qualified Control.Applicative
import Control.Monad.Exception.Synchronous
  (Exceptional(Success, Exception), switch )
import qualified Control.Monad.Exception.Synchronous as S
import Data.Functor.Identity ( runIdentity )
import Data.Text ( Text, pack, isPrefixOf, cons )
import qualified Data.Set as Set
import Data.Set ( Set )
import Control.Monad ( when, MonadPlus(mzero, mplus) )
import Control.Monad.Trans.Class ( lift )
import Data.Maybe ( isNothing )
import Data.Monoid ( Monoid ( mempty, mappend ) )
import Data.Functor.Identity ( Identity )
import Control.Monad.Trans.Class ( MonadTrans )
import Control.Monad.IO.Class ( MonadIO ( liftIO ) )
import qualified Data.List as L

type Expecting = String
type Saw = String

data Error = Expected Expecting Saw
             | FromFail String
             | Replaced String
             | UnknownError

-- | Carries the internal state of the parser. The counter is a simple
-- way to determine whether the remaining list one ParseSt has been
-- modified from another. When parsers modify remaining, they
-- increment the counter.
data ParseSt s = ParseSt { pendingShort :: String
                         , remaining :: [String]
                         , sawStopper :: Bool
                         , userState :: s
                         , counter :: Int
                         , errors :: [Error]
                         } deriving (Show, Eq)

-- | Load up the ParseSt with an initial user state and a list of
-- commmand line arguments.
defaultState :: s -> [String] -> ParseSt s
defaultState s ts = ParseSt { pendingShort = ""
                            , remaining = ts
                            , sawStopper = False
                            , userState = s
                            , counter = 0
                            , errors = [] }

-- | Carries the result of each parse.
data Result a = Bad | Good a

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
data ParserT s m a =
  ParserT { runParserT :: ParseSt s -> m (Result a, ParseSt s) }

instance (Monad m) => Functor (ParserT s m) where
  fmap = parserMap

instance (Monad m) => Applicative (ParserT s m) where
  pure = good
  (<*>) = apply

instance (Monad m) => Monoid (ParserT s m a) where
  mempty = genericThrow
  mappend = choice

instance (Monad m) => Alternative (ParserT s m) where
  empty = genericThrow
  (<|>) = choice
  many = several

instance (Monad m) => Monad (ParserT s m) where
  (>>=) = combine
  return = good
  fail = throwString

instance (Monad m) => MonadPlus (ParserT s m) where
  mzero = genericThrow
  mplus = choice

instance MonadTrans (ParserT s) where
  lift = parserLift

instance (MonadIO m) => MonadIO (ParserT s m) where
  liftIO = parserIO

-- | @ParserS s a@ is a parser with user state s,
-- underlying monad Identity, and result type a.
type ParserS s a = ParserT s Identity a

-- | @Parser a@ is a parser with user state (),
-- underlying monad Identity, and result type a.
type Parser a = ParserT () Identity a

-- | Runs a parser that has a user state and an underlying monad
-- Identity.
parseS ::
  s
  -- ^ The initial user state
  
  -> [String]
  -- ^ Command line arguments

  -> ParserS s a
  -- ^ Parser to run
  
  -> (Exceptional [Error] a, s)
  -- ^ Success or failure, and the final user state
  
parseSE s ts p =
  let r = runIdentity (runParserT p (defaultState s ts))
      (result, st') = r
  in case result of
    Good g -> (Success g, userState st')
    Bad -> (Exception (errors st'), userState st')

-- | Runs a parser that has no user state and an underlying monad of
-- Identity and is parameterized on the error type.
parse ::
  [String]
  -- ^ Command line arguments to parse
  
  -> Parser a
  -- ^ Parser to run
  
  -> Exceptional [Error] a
  -- ^ Success or failure

parseE ts p =
  let r = runIdentity (runParserT p (defaultState () ts))
      (result, st') = r
  in case result of
    Good g -> Success g
    Bad -> Exception (errors st')


-- | The most complex parser runner. Runs a parser with a user-defined
-- state, error type, and underlying monad. Returns the final parse
-- result and the final user state, inside of the underlying monad.
parseT ::
  (Monad m)
  => s
  -- ^ Initial user state

  -> [String]
  -- ^ Command line arguments to parse

  -> ParserT s m a
  -- ^ Parser to run
  
  -> m (Exceptional [Error] a, s)
  -- ^ Success or failure and the final user state, inside of the
  -- underlying monad

parseT s ts p = runParserT p (defaultState s ts) >>= \r ->
  let (result, st') = r
  in case result of
    Good g -> return (Success g, userState st')
    Bad -> return (Exception (errors st'), userState st')

-- | Lifts a computation of the underlying monad into the ParserT
-- monad. This provides the implementation for
-- 'Control.Monad.Trans.Class.lift'.
parserLift ::
  Monad m
  => m a
  -> ParserT s m a
parserLift c = ParserT $ \s ->
  c >>= \a -> return (Good a, s)

-- | Lifts a computation from the IO monad into the ParserT
-- monad. This provides the implementation for
-- 'Control.Monad.IO.Class.liftIO'.
parserIO ::
  MonadIO m
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
  Monad m
  => ParserT s m a
  -> (a -> ParserT s m b)
  -> ParserT s m b
combine (ParserT l) f = ParserT $ \s ->
  l s >>= \(r, s') ->
  case r of
    Bad -> return (Bad, s')
    (Good g) ->
      let (ParserT fr) = f g
      in fr s'

-- | @lookAhead p@ runs parser p. If p succeeds, lookAhead p succeeds
-- without consuming any input. If p fails without consuming any
-- input, so does lookAhead. If p fails and consumes input, lookAhead
-- also fails and consumes input. If this is undesirable, combine with
-- "try".
lookAhead ::
  Monad m
  => ParserT s m a
  -> ParserT s m a
lookAhead (ParserT p) = ParserT $ \s ->
  p s >>= \(r, s') ->
  return $ case r of
    Good g -> (Good g, s)
    Bad -> (Bad, s')

-- | @good a@ always succeeds without consuming any input and has
-- result a. This provides the implementation for
-- 'Control.Monad.Monad.return' and
-- 'Control.Applicative.Applicative.pure'.
good ::
  Monad m
  => a
  -> ParserT s m a
good a = ParserT $ \s ->
  return (Good a, s)

-- | @throwString s@ always fails without consuming any input. The
-- failure contains a record of the string passed in by s. This
-- provides the implementation for 'Control.Monad.Monad.fail'.
throwString ::
  Monad m
  => String
  -> ParserT s m a
throwString e = ParserT $ \s ->
  let err = FromFail e
      s' = s { errors = err : errors s }
  in return (Bad, s')


-- | @parserMap f p@ applies function f to the result of parser
-- p. First parser p is run. If it succeeds, function f is applied to
-- the result and another parser is returned with the result. If it
-- fails, f is not applied and a failed parser is returned. This
-- provides the implementation for 'Prelude.Functor.fmap'.
parserMap ::
  (Monad m)
  => (a -> b)
  -> ParserT s m a
  -> ParserT s m b
parserMap f (ParserT l) = ParserT $ \s ->
  l s >>= \r ->
  let (result, st') = r
  in case result of
    Good g -> return (Good (f g), st')
    Bad -> return (Bad, st')

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
  => ParserT s m (a -> b)
  -> ParserT s m a
  -> ParserT s m b
apply (ParserT x) (ParserT y) = ParserT $ \s ->
  x s >>= \r ->
  let (result, st') = r
  in case result of
    Good f -> y st' >>= \r' ->
      let (result', st'') = r'
      in case result' of
        Good a -> return (Good (f a), st'')
        Bad -> return (Bad, st'')
    Bad -> return (Bad, st')

-- | Fail with an unhelpful error message. Usually throw is more
-- useful, but this is handy to implement some typeclass instances.
genericThrow :: Monad m => ParserT s m a
genericThrow = throw UnknownError

-- | throw e always fails without consuming any input and returns a
-- failed parser with error state e.
throw :: (Monad m) => Error -> ParserT s e m a
throw e = ParserT $ \s ->
  return (Bad, s { errors = e : errors s })

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
  => ParserT s m a
  -> ParserT s m a
  -> ParserT s m a
choice (ParserT l) (ParserT r) = ParserT $ \sOld ->
  l sOld >>= \(a, s') ->
  case a of
    Good g ->
      let s'' = s' { errors = [] }
      in return (Good g, s'')
    Bad ->
      if noConsumed sOld s'
      then let s'' = sOld { errors = errors s' }
           in r s''
      else return (Bad, s')

-- | Runs the parser given. If it succeeds, then returns the result of
-- the parser. If it fails and consumes input, returns the result of
-- the parser. If it fails without consuming any input, then changes
-- the error using the function given.
--
-- If the parser fails without consuming any input but the user state
-- has changed, the parser returned will reflect the changed user
-- state.
(<?>) ::
  (Monad m)
  => ParserT s m a
  -> String
  -> ParserT s m a
(<?>) (ParserT l) e = ParserT $ \s ->
  l s >>= \(r, s') ->
  case r of
    Good g -> return (Good g, s')
    (Bad err) ->
      if noConsumed s s'
      then let s'' = s' { error = [Replaced e] }
           in return (Bad, s'')
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

pendingShortOpt :: Monad m => ShortOpt -> ParserT s m ShortOpt
pendingShortOpt so = ParserT $ \s ->
  let err saw = Expected ("short option: " ++ [(unShortOpt so)]) saw
      es saw = return (Bad, s { errors = err saw : errors s })
      gd (res, newSt) = return (Good res, newSt)
  in switch es gd $ do
    when (sawStopper s) $ S.throw "stopper"
    (first, rest) <- case pendingShort s of
      [] -> (S.throw "no pending short options") 
      x:xs -> return (x, xs)
    when (unShortOpt so /= first)
      (S.throw ("wrong pending short option: " ++ [first]))
    return (so, increment s { pendingShort = rest })

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
nonPendingShortOpt :: Monad m => ShortOpt -> ParserT s e m ShortOpt
nonPendingShortOpt so = ParserT $ \s ->
  let err saw = Expected $ msg ++ [unShortOpt so]
      msg = "non pending short option"
      errRet saw = return (Bad, s { errors = err saw : errors s })
      gd (g, n) = return (Good g, n)
  in switch errRet gd $ do
    checkPendingShorts s
    checkStopper s
    (a:s') <- nextWord s
    (maybeDash, word) <- case a of
      [] -> S.throw "zero length word"
      x:xs -> return (x, xs)
    when (maybeDash /= '-') $ S.throw "not an option"
    (letter, arg) <- case word of
      [] -> S.throw "argument is a single dash"
      x:xs -> return (x, xs)
    when (letter /= unShortOpt so) $
      S.throw ("different short option: " ++ [unShortOpt so])
    when (letter == '-') $ S.throw "new stopper"
    let s'' = s' { pendingShort = arg }
    return (so, s'')

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
  Monad m
  => LongOpt
  -> ParserT s m (LongOpt, Maybe String)
exactLongOpt lo = ParserT $ \s ->
  let ert saw = return (Bad, err saw)
      err saw = s { errors = Expected msg saw : errors s } where
        msg = "long option: " ++ unLongOpt lo
      gd (g, n) = return (Good g, n)
  in switch ert gd $ do
    checkPendingShorts s
    checkStopper s
    x:s' <- nextWord s
    (word, afterEq) <- getLongOption x
    when (word /= unLongOpt lo) $
      E.throw ("wrong long option: " ++ unLongOpt lo)
    return ((lo, afterEq), s')

-- | Takes a single String and returns a tuple, where the first element
-- is the first two letters, the second element is everything from the
-- third letter to the equal sign, and the third element is Nothing if
-- there is no equal sign, or Just String with everything after the
-- equal sign if there is one.
splitLongWord :: String -> (String, String, Maybe String)
splitLongWord t = (f, s, r) where
  (f, rest) = L.splitAt 2 t
  (s, withEq) = L.break (== '=') rest
  r = case withEq of
    [] -> Nothing
    _:xs -> xs


checkPendingShorts :: ParseSt s -> Exceptional String ()
checkPendingShorts st = case pendingShort st of
  [] -> return ()
  str -> S.throw $ "pending short options: " ++ str

checkStopper :: ParseSt s -> Exceptional String ()
checkStopper st = when (sawStopper st) (S.throw "stopper")

getLongOption ::
  String
  -> Exceptional String (String, Maybe String)
getLongOption str = do
  let (pre, word, afterEq) = splitLongWord str
  when (pre /= pack "--") (S.throw $ "not a long option: " ++ str)
  when (null word && isNothing afterEq) (S.throw ("stopper"))
  return (word, afterEq)
  

nextWord :: ParseSt s -> Exceptional String (String, ParseSt s)
nextWord st = case remaining st of
  [] -> S.throw "no arguments left"
  x:xs ->
    let s' = increment s { remaining = xs }
    in return (x, s')

approxLongOptError ::
  Set LongOpt
  -> ParseSt s
  -> String
  -> ParseSt s
approxLongOptError set st str = st { errors = newE : errors st } where
  newE = Expected exp str
  exp = "a long option: " ++ longs
  longs = concat . L.intersperse ", " $ opts
  opts = fmap unLongOpt . Set.toList $ set

-- | Examines the next word. If it matches a Text in the set
-- unambiguously, returns a tuple of the word actually found and the
-- matching word in the set.
approxLongOpt ::
  Monad m
  => Set LongOpt
  -> ParserT s m (String, LongOpt, Maybe String)
approxLongOpt ts = ParserT $ \s ->
  let err saw = return (Bad, approxLongOptError ts s saw)
      gd (g, newSt) = return (Good g, newSt)
  in switch err gd $ do
    checkPendingShorts s
    x:s' <- nextWord s
    (word, afterEq) <- getLongOption x
    opt <- case makeLongOpt word of
      Nothing -> S.throw $ "word cannot be a long option: " ++ word
      Just o -> return o
    if Set.member opt ts
      then return (word, opt, afterEq)
      else do
      let p t = word `isPrefixOf` (unLongOpt t)
          matches = Set.filter p ts
      case Set.toList matches of
      [] -> S.throw ("no matching option: " ++ word)
      (m:[]) -> let s' = increment s { remaining = xs }
                in return ((word, m, afterEq), s')
      _ -> S.throw ("ambiguous word: " ++ word)

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
pendingShortOptArg :: Monad m => ParserT s m String
pendingShortOptArg = ParserT $ \s ->
  let ert saw = return (Bad, err saw)
      err saw = s { errors = Expected msg saw : errors s } where
        msg = "pending short option argument"
      gd (g, newSt) = return (Good g, newSt)
  in switch ert gd $ do
    checkStopper s
    case pendingShort s of
      [] -> S.throw "no pending short option argument"
      xs ->
        let newSt = increment s { pendingShort = "" }
        in return (xs, newSt)


-- | Parses a "stopper" - that is, a double dash. Changes the internal
-- state of the parser to reflect that a stopper has been seen.
stopper :: Monad m => ParserT s m ()
stopper = ParserT $ \s ->
  let err saw = s { errors = Expected msg saw : errors s } where
        msg = "stopper"
      ert saw = return (Bad, err saw)
      gd (g, newSt) = return (Good g, newSt)
  in switch ert gd $ do
    checkPendingShorts s
    checkStopper s
    (x, s') <- nextWord
    when (not $ pack "--" `isPrefixOf` x) (S.throw E.SawNotStopper)
    when (X.length x /= 2) (S.throw E.SawNotStopper)
    let s'' = s' { sawStopper = True }
    return ((), s'')

-- | try p behaves just like p, but if p fails, try p will not consume
-- any input. However, the user state reflects any changes that parser
-- p made to it.
try :: Monad m => ParserT s m a -> ParserT s m a
try (ParserT l) = ParserT $ \s ->
  l s >>= \(r, s') ->
  case r of
    (Good g) -> return (Good g, s')
    Bad -> return (Bad, s'') where
      s'' = s { errors = errors s'
              , userState = userState s' }

-- | Returns the next string on the command line as long as there are
-- no pendings. Be careful - this will return the next string even if
-- it looks like an option (that is, it starts with a dash.) Consider
-- whether you should be using nonOptionPosArg instead. However this
-- can be useful when parsing command line options after a stopper.
nextArg :: Monad m => ParserT s m String
nextArg = ParserT $ \s ->
  let ert saw = return (Bad, err saw)
      err saw = s { errors = Expected msg saw : errors s } where
        msg = "next argument"
      gd (g, newSt) = return (Good g, newSt)
  in switch ert gd $ do
    checkPendingShorts s
    nextWord


-- | Returns the next string on the command line as long as there are
-- no pendings and as long as the next string does not begin with a
-- dash. If there has already been a stopper, then will return the
-- next string even if it starts with a dash.
nonOptionPosArg ::
  (E.Error e, Monad m)
  => ParserT s e m Text
nonOptionPosArg = ParserT $ \s ->
  let ert saw = return (Bad, err saw)
      err saw = s { errors = Expected msg saw : errors s } where
        msg = "non option positional argument"
      gd (g, newSt) = return (Good g, newSt)
  in switch ert gd $ do
    checkPendingShorts s
    (x, s') <- nextWord s
    result <-
      if sawStopper s
      then return x
      else case x of
        [] -> return x
        f:_ -> if f == '-'
               then S.throw $ "argument with leading dash: " ++ x
               else return x
    return (result, s')


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
  => ParserT s m a
  -> ParserT s m end
  -> ParserT s m [a]
manyTill (ParserT r) (ParserT f) = ParserT $ \s ->
  parseTill s r f >>= \till ->
  return $ if lastRunFailed till
           then (Bad, lastSt till)
           else (Good (goods till), lastSt till)


data Till s a =
  Till { goods :: [a]
       , lastSt :: ParseSt s
       , lastRunFailed :: Bool }

parseTill ::
  (Monad m)
  => ParseSt s
  -> (ParseSt s -> m (Result a, ParseSt s))
  -> (ParseSt s -> m (Result b, ParseSt s))
  -> m (Till s a)
parseTill s fr ff = ff s >>= \r ->
  case r of
    (Good _, s') -> return $ Till [] s' False
    (Bad, _) ->
      fr s >>= \r' ->
      case r' of
        (Bad, s') -> return $ Till [] s' True
        (Good g, s') ->
          parseTill s' fr ff >>= \r'' ->
          let (Till gs lS lF) = r''
          in if counter s' == counter s
             then error "parseTill applied to parser that takes empty list"
             else return $ Till (g:gs) lS lF

-- It is impossible to implement several and manyTill in terms of
-- feed, as there is no easy way to specify what the starting value of
-- the state for feed would be.

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
--
-- This function provides the implementation for
-- 'Control.Applicative.Alternative.many'.
several ::
  (Monad m)
  => ParserT s m a
  -> ParserT s m [a]
several (ParserT l) = ParserT $ \s ->
  parseRepeat s l >>= \r ->
  let (result, finalGoodSt, finalBadSt) = r
  in if noConsumed finalGoodSt finalBadSt
     then return (Good result, finalGoodSt)
     else return (Bad, finalBadSt)

parseRepeat ::
  (Monad m)
  => ParseSt s
  -> (ParseSt s -> m (Result a, ParseSt s))
  -> m ([a], ParseSt s, ParseSt s)
parseRepeat st1 f = f st1 >>= \r ->
  case r of
    (Good a, st') ->
      if noConsumed st1 st'
      then error $ "several applied to parser that succeeds without"
           ++ " consuming any input"
      else
        parseRepeat st' f >>= \r' ->
        let (ls, finalGoodSt, finalBadSt) = r'
        in return (a : ls, finalGoodSt, finalBadSt)
    (Bad, st') -> return ([], st1, st')

-- | feed runs in a recursive loop. Each loop starts with three
-- variables: a function @f@ that takes an input @i@ and returns a
-- parser @p@, a function @g@ that takes an input @i@ and returns a
-- parser @e@ that must succeed for the recursion to end, and an
-- initial input @i@. This proceeds as follows:
--
-- 1. Apply @g@ to @i@ and run resulting parser @e@. If this parser
-- succeeds, feed succeeds and returns a list of all successful runs
-- of @p@. The result of @e@ is not returned, but otherwise the parser
-- returned reflects the updated internal parser state from the
-- running of @e@. (If that is a problem, wrap @e@ in 'lookAhead'.) If
-- @e@ fails and consumes input, feed fails and returns a failed
-- parser whose internal state reflects the state after @e@ fails. If
-- @e@ fails without consuming any input, proceed with the following
-- steps.
--
-- 2. Apply function @f@ to input @i@, yielding a parser @p@. Run
-- parser @p@. If @p@ fails, feed also fails. If @p@ succeeds, it
-- yields a new input, @i'@.
--
-- 3. If @p@ succeeded without consuming any input, an infinite loop
-- will result, so apply @error@.
--
-- 4. Repeat from step 1, but with the new input retured from @p@,
-- @i'@.
--
-- For the initial application of feed, you supply the function @f@,
-- the end parser @e@, and the initial state @i@.
--
-- This function is useful for running multiple times a parser that
-- depends on the result of previous runs of the parser. You could
-- implement something similar using the user state feature, but for
-- various reasons sometimes it is more useful to use 'feed' instead.

feed ::
  Monad m
  => (a -> ParserT s m a)
  -> (a -> ParserT s m end)
  -> a
  -> ParserT s m [a]
feed f e i = ParserT $ \s ->
  feedRecurse s f e i >>= \(s', lf) ->
  let res = case lf of
        EndFailure -> Bad
        RepeatFailure -> Bad
        RepeatSuccess ls -> Good ls
  in return (res, s')

data LastFeed a =
  EndFailure
  -- ^ The last run of the end parser failed and consumed input.
  
  | RepeatFailure
    -- ^ The last run of the repetitive parser failed (whether or not
    -- it consumed any input).

  | RepeatSuccess [a]
    -- ^ The last run of the repetitive parser succeeded; here are all
    -- the results.

-- | Takes an initial state, a function @f@ that returns a parser @p@
-- to run repetitively, a parser @e@ that must succeed to stop the
-- recursion, and an input for @f@. Returns: @m (s, ei)@, where: 
--
-- * @m@ is the inner monad
--
-- * @s@ is the state after the last application of either @e@ or @p@.
--
-- * @lf@ is a 'LastFeed' (see above).
feedRecurse ::
  Monad m
  => ParseSt s
  -> (a -> ParserT s m a)
  -> (a -> ParserT s m end)
  -> a
  -> m (ParseSt s, LastFeed a)
feedRecurse st f fe i =
  runParserT (fe i) st >>= \(eResult, eSt) ->
  case eResult of
    Good _ -> return (eSt, RepeatSuccess [])
    Bad ->
      if noConsumed st eSt
      then
        runParserT (f i) st >>= \(pResult, pSt) ->
        case pResult of
          Good g ->
            if noConsumed st pSt
            then feedRecurseError
            else
              feedRecurse pSt f fe g >>= \(recSt, lf) ->
              let res = case lf of
                    RepeatSuccess ls -> RepeatSuccess (g:ls)
                    failed -> failed
              in return (recSt, res)
          Bad -> return (pSt, RepeatFailure)
      else
        return (eSt, EndFailure)

feedRecurseError :: a
feedRecurseError =
  error $ "feedRecurse applied to parser that succeeds without"
  ++ " consuming any input"


-- | Succeeds if there is no more input left.
end :: Monad m => ParserT s m ()
end = ParserT $ \s ->
  let ert saw = return (Bad, err saw)
      err saw = s { errors = Expected msg saw : errors s } where
        msg = "end of input"
      gd (g, newSt) = return (Good g, newSt)
  in switch ert gd $ do
    checkPendingShorts s
    when (not . null . remaining $ s) (S.throw "more input")
    return ((), s)

-- | Gets the user state.
get :: Monad m => ParserT s m s
get = ParserT $ \s ->
  return (Good (userState s), s)

-- | Puts a new user state.
put :: Monad m => s -> ParserT s m ()
put newUserSt = ParserT $ \s ->
  return (Good (), s { userState = newUserSt })

-- | Modify the user state.
modify :: Monad m => (s -> s) -> ParserT s m ()
modify f = ParserT $ \s ->
  return (Good (), s { userState = f (userState s) })
