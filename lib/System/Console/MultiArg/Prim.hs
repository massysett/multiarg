-- | Parser primitives. These are the only functions that have access
-- to the internals of the parser. Use these functions if you want to
-- build your own parser from scratch. If your needs are simpler, you
-- will want to look at "System.Console.MultiArg.SimpleParser" or
-- "System.Console.MultiArg.Combinator", which do a lot of grunt work
-- for you.
module System.Console.MultiArg.Prim (
    -- * Parser types
  Parser,
  
  -- * Running a parser
  
  -- | Each parser runner is applied to a list of Strings, which are the
  -- command line arguments to parse. If there is any chance that you
  -- will be parsing Unicode strings, see the documentation in
  -- "System.Console.MultiArg.GetArgs" before you use
  -- 'System.Environment.getArgs'.
  parse,
  
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
  resetStopper,
  
  -- ** Positional (non-option) arguments
  nextArg,
  nonOptionPosArg,
  
  -- ** Miscellaneous
  end,
  
  -- * Errors
  Message(Expected, FromFail, Replaced, UnknownError),
  Expecting,
  Saw

  ) where


import System.Console.MultiArg.Option
  (ShortOpt,
    unShortOpt,
    LongOpt,
    unLongOpt, 
    makeLongOpt )
import Control.Applicative ( Applicative, Alternative )
import qualified Control.Applicative
import Control.Monad.Exception.Synchronous
  (Exceptional(Success, Exception), switch )
import qualified Control.Monad.Exception.Synchronous as S
import qualified Data.Set as Set
import Data.Set ( Set )
import Control.Monad ( when, MonadPlus(mzero, mplus) )
import Data.Maybe ( isNothing )
import Data.Monoid ( Monoid ( mempty, mappend ) )
import qualified Data.List as L
import Data.List (isPrefixOf)

type Location = String

-- | An Error contains a list of Messages and a String indicating
-- where the error happened.
data Error = Error [Message] Location deriving Show

-- | Error messages.
data Message =
  Expected String
  -- ^ The parser expected to see one thing, but it actually saw
  -- something else. The string indicates what was expected.
  | FromFail String
    -- ^ The 'fail' function was applied.
    
  | Replaced String
    -- ^ A previous list of error messages was replaced with this error message.
    
  | UnknownError
    -- ^ Any other error; used by 'genericThrow'.

  deriving Show

-- | Carries the internal state of the parser. The counter is a simple
-- way to determine whether the remaining list one ParseSt has been
-- modified from another. When parsers modify remaining, they
-- increment the counter.
data ParseSt = ParseSt { pendingShort :: String
                       , remaining :: [String]
                       , sawStopper :: Bool
                       , counter :: Int
                       , errors :: [Message]
                       } deriving Show

-- | Load up the ParseSt with an initial user state and a list of
-- commmand line arguments.
defaultState :: [String] -> ParseSt
defaultState ts = ParseSt { pendingShort = ""
                          , remaining = ts
                          , sawStopper = False
                          , counter = 0
                          , errors = [] }

-- | Carries the result of each parse.
data Result a = Bad | Good a

-- | Parsers. Internally the parser tracks what input remains to be
-- parsed, whether there are any pending short options, and whether a
-- stopper has been seen. A parser can return a value of any type.
--
-- The parser also includes the notion of failure. Any parser can
-- fail; a failed parser affects the behavior of combinators such as
-- combine.
data Parser a =
  Parser { runParser :: ParseSt -> (Result a, ParseSt) }

instance Functor Parser where
  fmap = parserMap

instance Applicative Parser where
  pure = good
  (<*>) = apply

instance Monoid (Parser a) where
  mempty = genericThrow
  mappend = choice

instance Alternative Parser where
  empty = genericThrow
  (<|>) = choice
  many = several

instance Monad Parser where
  (>>=) = combine
  return = good
  fail = throwString

instance MonadPlus Parser where
  mzero = genericThrow
  mplus = choice


-- | Runs a parser. This is the only way to change a value of type
-- @Parser a@ into a value of type @a@ (that is, it is the only way to
-- \"get out of the Parser monad\" or to \"escape the Parser monad\".)
parse ::
  [String]
  -- ^ Command line arguments to parse
  
  -> Parser a
  -- ^ Parser to run
  
  -> Exceptional Error a
  -- ^ Success or failure. Any parser might fail; for example, the
  -- command line might not have any values left to parse. Use of the
  -- @<|>@ combinator can lead to a list of failures. If multiple
  -- parsers are tried one after another using the @<|>@ combinator,
  -- and each fails without consuming any input, then multiple Error
  -- will result, one for each failure.

parse ts p =
  let (result, st') = runParser p (defaultState ts)
  in case result of
    Good g -> Success g
    Bad -> Exception (errors st')


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
combine :: Parser a -> (a -> Parser b) -> Parser b
combine a k = Parser $ \s ->
  let (r, s') = runParser a s
  in case r of
    Bad -> (Bad, s')
    Good g -> runParser (k g) s'


-- | @lookAhead p@ runs parser p. If p succeeds, lookAhead p succeeds
-- without consuming any input. If p fails without consuming any
-- input, so does lookAhead. If p fails and consumes input, lookAhead
-- also fails and consumes input. If this is undesirable, combine with
-- "try".
lookAhead :: Parser a -> Parser a
lookAhead a = Parser $ \s ->
  let (r, s') = runParser a s
  in case r of
    Good g -> (Good g, s)
    Bad -> (Bad, s')


-- | @good a@ always succeeds without consuming any input and has
-- result a. This provides the implementation for
-- 'Control.Monad.Monad.return' and
-- 'Control.Applicative.Applicative.pure'.
good :: a -> Parser a
good a = Parser $ \s -> (Good a, s)


-- | @throwString s@ always fails without consuming any input. The
-- failure contains a record of the string passed in by s. This
-- provides the implementation for 'Control.Monad.Monad.fail'.
throwString :: String -> Parser a
throwString e = Parser $ \s ->
  let err = FromFail e
      s' = s { errors = err : errors s }
  in (Bad, s')


-- | @parserMap f p@ applies function f to the result of parser
-- p. First parser p is run. If it succeeds, function f is applied to
-- the result and another parser is returned with the result. If it
-- fails, f is not applied and a failed parser is returned. This
-- provides the implementation for 'Prelude.Functor.fmap'.
parserMap :: (a -> b) -> Parser a -> Parser b
parserMap f l = Parser $ \s ->
  let (r, s') = runParser l s
  in case r of
    Good g -> (Good (f g), s')
    Bad -> (Bad, s')


-- | apply l r applies the function found in parser l to the result of
-- parser r. First the l parser is run. If it succeeds, it has a
-- resulting function. Then the r parser is run. If it succeeds, the
-- function from the l parser is applied to the result of the r
-- parser, and a new parser is returned with the result. If either
-- parser l or parser r fails, then a failed parser is returned. This
-- provides the implementation for '<*>' in
-- 'Control.Applicative.Applicative'.
apply :: Parser (a -> b) -> Parser a -> Parser b
apply fa a = Parser $ \s ->
  let (r, s') = runParser fa s
  in case r of
    Good g ->
      let (ra, sa) = runParser a s'
      in case ra of
        Good ga -> (Good (g ga), sa)
        Bad -> (Bad, sa)
    Bad -> (Bad, s')


-- | Fail with an unhelpful error message. Usually throw is more
-- useful, but this is handy to implement some typeclass instances.
genericThrow :: Parser a
genericThrow = throw UnknownError

-- | throw e always fails without consuming any input and returns a
-- failed parser with error state e.
throw :: Message -> Parser a
throw e = Parser $ \s ->
  (Bad, s { errors = e : errors s })

noConsumed :: ParseSt -> ParseSt -> Bool
noConsumed old new = counter old >= counter new

-- | Runs the first parser. If it fails without consuming any input,
-- then runs the second parser. If the first parser succeeds, then
-- returns the result of the first parser. If the first parser fails
-- and consumes input, then returns the result of the first
-- parser. This provides the implementation for
-- '<|>' in 'Control.Applicative.Alternative'.
choice :: Parser a -> Parser a -> Parser a
choice a b = Parser $ \sOld ->
  let (ra, sa) = runParser a sOld
  in case ra of
    Good g ->
      let sNew = sa { errors = [] }
      in (Good g, sNew)
    Bad ->
      if noConsumed sOld sa
      then let sNew = sOld { errors = errors sa }
           in runParser b sNew
      else (Bad, sa)


-- | Runs the parser given. If it succeeds, then returns the result of
-- the parser. If it fails and consumes input, returns the result of
-- the parser. If it fails without consuming any input, then changes
-- the error using the function given.
(<?>) :: Parser a -> String -> Parser a
(<?>) l e = Parser $ \s ->
  let (r, s') = runParser l s
  in case r of
    Good g -> (Good g, s')
    Bad ->
      if noConsumed s s'
      then let s'' = s' { errors = [Replaced e] }
           in (Bad, s'')
      else (Bad, s')

infix 0 <?>

increment :: ParseSt -> ParseSt
increment old = old { counter = succ . counter $ old }

-- | Parses only pending short options. Fails without consuming any
-- input if there has already been a stopper or if there are no
-- pending short options. Fails without consuming any input if there
-- is a pending short option, but it does not match the short option
-- given. Succeeds and consumes a pending short option if it matches
-- the short option given; returns the short option parsed.

pendingShortOpt :: ShortOpt -> Parser ()
pendingShortOpt so = Parser $ \s ->
  let err saw = Expected ("short option: " ++ [(unShortOpt so)]) saw
      es saw = (Bad, s { errors = err saw : errors s })
      gd newSt = (Good (), newSt)
  in switch es gd $ do
    when (sawStopper s) $ S.throw "stopper"
    (first, rest) <- case pendingShort s of
      [] -> (S.throw "no pending short options") 
      x:xs -> return (x, xs)
    when (unShortOpt so /= first)
      (S.throw ("wrong pending short option: " ++ [first]))
    return (increment s { pendingShort = rest })

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
-- from remaining arguments to be parsed.
nonPendingShortOpt :: ShortOpt -> Parser ()
nonPendingShortOpt so = Parser $ \s ->
  let err saw = Expected (msg ++ [unShortOpt so]) saw
      msg = "non pending short option"
      errRet saw = (Bad, s { errors = err saw : errors s })
      gd n = (Good (), n)
  in switch errRet gd $ do
    checkPendingShorts s
    checkStopper s
    (a, s') <- nextWord s
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
    return s''

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
exactLongOpt :: LongOpt -> Parser (Maybe String)
exactLongOpt lo = Parser $ \s ->
  let ert saw = (Bad, err saw)
      err saw = s { errors = Expected msg saw : errors s } where
        msg = "long option: " ++ unLongOpt lo
      gd (g, n) = (Good g, n)
  in switch ert gd $ do
    checkPendingShorts s
    checkStopper s
    (x, s') <- nextWord s
    (word, afterEq) <- getLongOption x
    when (word /= unLongOpt lo) $
      S.throw ("wrong long option: " ++ unLongOpt lo)
    return (afterEq, s')

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
    _:xs -> Just xs


checkPendingShorts :: ParseSt -> Exceptional String ()
checkPendingShorts st = case pendingShort st of
  [] -> return ()
  str -> S.throw $ "pending short options: " ++ str

checkStopper :: ParseSt -> Exceptional String ()
checkStopper st = when (sawStopper st) (S.throw "stopper")

getLongOption :: String -> Exceptional String (String, Maybe String)
getLongOption str = do
  let (pre, word, afterEq) = splitLongWord str
  when (pre /= "--") (S.throw $ "not a long option: " ++ str)
  when (null word && isNothing afterEq) (S.throw ("stopper"))
  return (word, afterEq)
  

nextWord :: ParseSt -> Exceptional String (String, ParseSt)
nextWord st = case remaining st of
  [] -> S.throw "no arguments left"
  x:xs ->
    let s' = increment st { remaining = xs }
    in return (x, s')

approxLongOptError ::
  Set LongOpt
  -> ParseSt
  -> String
  -> ParseSt
approxLongOptError set st str = st { errors = newE : errors st } where
  newE = Expected ex str
  ex = "a long option: " ++ longs
  longs = concat . L.intersperse ", " $ opts
  opts = fmap unLongOpt . Set.toList $ set

-- | Examines the next word. If it matches a Text in the set
-- unambiguously, returns a tuple of the word actually found and the
-- matching word in the set. If the Set is empty, this parser will
-- always fail.
approxLongOpt ::
  Set LongOpt
  -> Parser (String, LongOpt, Maybe String)
approxLongOpt ts = Parser $ \s ->
  let err saw = (Bad, approxLongOptError ts s saw)
      gd (g, newSt) = (Good g, newSt)
  in switch err gd $ do
    checkPendingShorts s
    (x, s') <- nextWord s
    (word, afterEq) <- getLongOption x
    opt <- case makeLongOpt word of
      Nothing -> S.throw $ "word cannot be a long option: " ++ word
      Just o -> return o
    if Set.member opt ts
      then return ((word, opt, afterEq), s')
      else do
      let p t = word `isPrefixOf` (unLongOpt t)
          matches = Set.filter p ts
      case Set.toList matches of
        [] -> S.throw ("no matching option: " ++ word)
        (m:[]) -> return ((word, m, afterEq), s')
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
pendingShortOptArg :: Parser String
pendingShortOptArg = Parser $ \s ->
  let ert saw = (Bad, err saw)
      err saw = s { errors = Expected msg saw : errors s } where
        msg = "pending short option argument"
      gd (g, newSt) = (Good g, newSt)
  in switch ert gd $ do
    checkStopper s
    case pendingShort s of
      [] -> S.throw "no pending short option argument"
      xs ->
        let newSt = increment s { pendingShort = "" }
        in return (xs, newSt)


-- | Parses a "stopper" - that is, a double dash. Changes the internal
-- state of the parser to reflect that a stopper has been seen.
stopper :: Parser ()
stopper = Parser $ \s ->
  let err saw = s { errors = Expected msg saw : errors s } where
        msg = "stopper"
      ert saw = (Bad, err saw)
      gd (g, newSt) = (Good g, newSt)
  in switch ert gd $ do
    checkPendingShorts s
    checkStopper s
    (x, s') <- nextWord s
    when (not $ "--" `isPrefixOf` x)
      (S.throw "not a stopper")
    when (length x /= 2)
      (S.throw "not a stopper")
    let s'' = s' { sawStopper = True }
    return ((), s'')

-- | If a stopper has already been seen, change the internal state
-- back to indicating that no stopper has been seen.
resetStopper :: Parser ()
resetStopper = Parser $ \s ->
  let s' = s { sawStopper = False }
  in (Good (), s')

-- | try p behaves just like p, but if p fails, try p will not consume
-- any input.
try :: Parser a -> Parser a
try a = Parser $ \s ->
  let (r, s') = runParser a s
  in case r of
    Good g -> (Good g, s')
    Bad -> (Bad, s'') where
      s'' = s { errors = errors s' }
      

-- | Returns the next string on the command line as long as there are
-- no pendings. Be careful - this will return the next string even if
-- it looks like an option (that is, it starts with a dash.) Consider
-- whether you should be using nonOptionPosArg instead. However this
-- can be useful when parsing command line options after a stopper.
nextArg :: Parser String
nextArg = Parser $ \s ->
  let ert saw = (Bad, err saw)
      err saw = s { errors = Expected msg saw : errors s } where
        msg = "next argument"
      gd (g, newSt) = (Good g, newSt)
  in switch ert gd $ do
    checkPendingShorts s
    nextWord s


-- | Returns the next string on the command line as long as there are
-- no pendings and as long as the next string does not begin with a
-- dash. If there has already been a stopper, then will return the
-- next string even if it starts with a dash.
nonOptionPosArg :: Parser String
nonOptionPosArg = Parser $ \s ->
  let ert saw = (Bad, err saw)
      err saw = s { errors = Expected msg saw : errors s } where
        msg = "non option positional argument"
      gd (g, newSt) = (Good g, newSt)
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
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill (Parser r) (Parser f) = Parser $ \s ->
  let Till g lS lF = parseTill s r f
  in if lF then (Bad, lS) else (Good g, lS)


data Till a =
  Till { _goods :: [a]
       , _lastSt :: ParseSt
       , _lastRunFailed :: Bool }

parseTill ::
  ParseSt
  -> (ParseSt -> (Result a, ParseSt))
  -> (ParseSt -> (Result b, ParseSt))
  -> Till a
parseTill s fr ff =
  case ff s of
    (Good _, s') -> Till [] s' False
    (Bad, _) ->
      case fr s of
        (Bad, s'') -> Till [] s'' True
        (Good g, s'') ->
          let Till gs lS lF = parseTill s'' fr ff
          in if counter s'' == counter s
             then parseTillErr
             else Till (g:gs) lS lF

parseTillErr :: a
parseTillErr =
  error "parseTill applied to parser that takes empty list"


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
several :: Parser a -> Parser [a]
several (Parser l) = Parser $ \s ->
  let (result, finalGoodSt, finalBadSt) = parseRepeat s l
  in if noConsumed finalGoodSt finalBadSt
     then (Good result, finalGoodSt)
     else (Bad, finalBadSt)


parseRepeat ::
  ParseSt
  -> (ParseSt -> (Result a, ParseSt))
  -> ([a], ParseSt, ParseSt)
parseRepeat st1 f =
  case f st1 of
    (Good a, st') ->
      if noConsumed st1 st'
      then error $ "several applied to parser that succeeds without"
           ++ " consuming any input"
      else
        let (ls, finalGoodSt, finalBadSt) = parseRepeat st' f
        in (a : ls, finalGoodSt, finalBadSt)
    (Bad, st') -> ([], st1, st')
    

-- | Succeeds if there is no more input left.
end :: Parser ()
end = Parser $ \s ->
  let ert saw = (Bad, err saw)
      err saw = s { errors = Expected msg saw : errors s } where
        msg = "end of input"
      gd (g, newSt) = (Good g, newSt)
  in switch ert gd $ do
    checkPendingShorts s
    when (not . null . remaining $ s) (S.throw "more input")
    return ((), s)
