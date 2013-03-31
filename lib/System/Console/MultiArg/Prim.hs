-- | Parser primitives. These are the only functions that have access
-- to the internals of the parser. Use these functions if you want to
-- build your own parser from scratch. If your needs are simpler, you
-- will want to look at "System.Console.MultiArg.SimpleParser" or
-- "System.Console.MultiArg.Combinator", which do a lot of grunt work
-- for you.
--
-- Internal design, especially the error handling, is based in large
-- part on Parsec, as described in the paper at
-- <http://legacy.cs.uu.nl/daan/pubs.html#parsec>.
module System.Console.MultiArg.Prim (
    -- * Parser types
  Parser,

  -- * Running a parser

  -- | Each parser runner is applied to a list of Strings, which are the
  -- command line arguments to parse.
  parse,

  -- * Higher-level parser combinators
  good,
  choice,
  bind,
  lookAhead,

  -- ** Running parsers multiple times
  several,
  several1,
  manyTill,

  -- ** Failure and errors
  failString,
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
  nextWord,
  nextWordIs,
  nonOptionPosArg,
  matchApproxWord,

  -- ** Miscellaneous
  end,

  -- * Errors
  Description(..),
  Error(Error),
  InputDesc

  ) where


import System.Console.MultiArg.Option
  (ShortOpt,
    unShortOpt,
    LongOpt,
    unLongOpt,
    makeLongOpt )
import Control.Applicative ( Applicative, Alternative, optional )
import qualified Control.Applicative as A
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Set as Set
import Data.Set ( Set )
import qualified Control.Monad
import Control.Monad ( when, MonadPlus(mzero, mplus), guard, liftM )
import Data.Maybe (mapMaybe)
import Data.Monoid ( Monoid ( mempty, mappend ) )
import qualified Data.List as L
import Data.List (isPrefixOf)

-- | Parsers. Internally the parser tracks what input remains to be
-- parsed, whether there are any pending short options, and whether a
-- stopper has been seen. A parser can return a value of any type.
--
-- The parser also includes the notion of failure. Any parser can
-- fail; a failed parser affects the behavior of combinators such as
-- choice.
newtype Parser a = Parser { runParser :: State -> Consumed a }

instance Monad Parser where
  (>>=) = bind
  return = good
  fail = failString

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  (<*>) = Control.Monad.ap
  pure = return

instance Alternative Parser where
  empty = genericThrow
  (<|>) = choice
  some = several1
  many = several

instance Monoid (Parser a) where
  mempty = genericThrow
  mappend = choice

instance MonadPlus Parser where
  mzero = genericThrow
  mplus = choice

type PendingShort = String
type Remaining = [String]
type SawStopper = Bool
data State = State PendingShort Remaining SawStopper

type InputDesc = String
data Description = Unknown | General String | Expected String
  deriving (Eq, Show, Ord)

-- | Error messages. To format error messages for nice display, see
-- 'System.Console.MultiArg.Combinator.formatError'.
data Error = Error InputDesc [Description]
  deriving (Eq, Show, Ord)

data Reply a = Ok a State Error
             | Fail Error

data Consumed a = Consumed (Reply a)
                | Empty (Reply a)

-- | @good a@ always succeeds without consuming any input and has
-- result a. This provides the implementation for
-- 'Control.Monad.Monad.return' and
-- 'Control.Applicative.Applicative.pure'.
good :: a -> Parser a
good x = Parser $ \st -> Empty (Ok x st (Error (descLocation st) []))

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
bind :: Parser a -> (a -> Parser b) -> Parser b
bind (Parser p) f = Parser $ \s ->
  case p s of
    Empty r1 -> case r1 of
      Ok x s' _ -> runParser (f x) s'
      Fail m -> Empty (Fail m)
    Consumed r1 -> Consumed $
      case r1 of
        Ok x s' _ -> case runParser (f x) s' of
          Consumed r -> r
          Empty r -> r
        Fail e -> Fail e

descLocation :: State -> InputDesc
descLocation (State ps rm st) = pending ++ next ++ stop
  where
    pending
      | null ps = ""
      | otherwise = "short option or short option argument: "
                  ++ ps ++ " "
    next = case rm of
      [] -> "no words remaining"
      x:_ -> "next word: " ++ x
    stop = if st then " (stopper already seen)" else ""


-- | @failString s@ always fails without consuming any input. The
-- failure contains a record of the string passed in by s. This
-- provides the implementation for 'Control.Monad.Monad.fail'.
failString :: String -> Parser a
failString str = Parser $ \s ->
  Empty (Fail (Error (descLocation s) [General str]))


-- | Fail with an unhelpful error message. Usually 'throwString' is
-- more useful, but this is handy to implement some typeclass
-- instances.
genericThrow :: Parser a
genericThrow = Parser $ \s ->
  Empty (Fail (Error (descLocation s) [Unknown]))

-- | Runs the first parser. If it fails without consuming any input,
-- then runs the second parser. If the first parser succeeds, then
-- returns the result of the first parser. If the first parser fails
-- and consumes input, then returns the result of the first
-- parser. This provides the implementation for
-- '<|>' in 'Control.Applicative.Alternative'.
choice :: Parser a -> Parser a -> Parser a
choice p q = Parser $ \s ->
  case runParser p s of
    Empty (Fail msg1) ->
      case runParser q s of
        Empty (Fail msg2) -> mergeError msg1 msg2
        Empty (Ok x s' msg2) -> mergeOk x s' msg1 msg2
        c -> c
    Empty (Ok x s' msg1) ->
      case runParser q s of
        Empty (Fail msg2) -> mergeOk x s' msg1 msg2
        Empty (Ok _ _ msg2) -> mergeOk x s' msg1 msg2
        c -> c
    c -> c
  where
    mergeOk x s msg1 msg2 = Empty (Ok x s (merge msg1 msg2))
    mergeError msg1 msg2 = Empty (Fail (merge msg1 msg2))
    merge (Error loc exp1) (Error _ exp2) =
      Error loc (exp1 ++ exp2)

-- | Applies 'error' if a parser would succeed without consuming any
-- input. Useful for preventing infinite loops on parsers like
-- 'several1'.
crashOnEmptyOk
  :: String
  -- ^ Use this label when applying 'error'

  -> Parser a
  -> Parser a
crashOnEmptyOk str p = Parser $ \s ->
  case runParser p s of
    Empty r -> case r of
      Ok _ _ _ ->
         error $ "multiarg: error: " ++ str
               ++ " applied to parser that succeeds without "
               ++ "consuming any input. Aborted to prevent "
               ++ "an infinite loop."
      e -> Empty e
    o -> o
               

-- | Runs a parser one or more times. Runs the parser once and then
-- applies 'several'.
several1 :: Parser a -> Parser [a]
several1 p = do
  r1 <- p
  rs <- several p
  return $ r1:rs


-- | Runs a parser zero or more times. If the last run of the parser
-- fails without consuming any input, this parser succeeds without
-- consuming any input. If the last run of the parser fails while
-- consuming input, this parser fails while consuming input. This
-- provides the implementation for 'many' in Control.Applicative.
several :: Parser a -> Parser [a]
several unwrapped =
  let p = crashOnEmptyOk "several" unwrapped
  in do
    maybeA <- optional p
    case maybeA of
      Nothing -> return []
      Just a -> do
        rest <- several unwrapped
        return $ a:rest
  

-- | Runs the parser given. If it fails without consuming any input,
-- replaces all Expected messages with the one given. Otherwise,
-- returns the result of the parser unchanged.
(<?>) :: Parser a -> String -> Parser a
p <?> str = Parser $ \s ->
  case runParser p s of
    Empty (Fail m) -> Empty (Fail (expect m str))
    Empty (Ok x s' m) -> Empty (Ok x s' (expect m str))
    x -> x
  where
    expect (Error pos ls) s =
      let ls' = mapMaybe notExpected ls
          notExpected d = case d of
            Expected _ -> Nothing
            x -> Just x
      in Error pos ((Expected s) : ls')

infix 0 <?>

-- | Runs a parser. This is the only way to change a value of type
-- @Parser a@ into a value of type @a@ (that is, it is the only way to
-- \"get out of the Parser monad\" or to \"escape the Parser monad\".)
parse
  :: [String]
  -- ^ Command line arguments to parse. Presumably you got these from
  -- 'getArgs'. If there is any chance that you will be parsing
  -- Unicode strings, see the documentation in
  -- "System.Console.MultiArg.GetArgs" before you use
  -- 'System.Environment.getArgs'.

  -> Parser a
  -- ^ Parser to run

  -> Ex.Exceptional Error a
  -- ^ Success or failure. Any parser might fail; for example, the
  -- command line might not have any values left to parse. Use of the
  -- 'choice' combinator can lead to a list of failures.

parse ss p =
  let s = State "" ss False
      procReply r = case r of
        Ok x _ _ -> Ex.Success x
        Fail m -> Ex.Exception m
  in case runParser p s of
      Consumed r -> procReply r
      Empty r -> procReply r

-- | Parses only pending short options. Fails without consuming any
-- input if there has already been a stopper or if there are no
-- pending short options. Fails without consuming any input if there
-- is a pending short option, but it does not match the short option
-- given. Succeeds and consumes a pending short option if it matches
-- the short option given.
pendingShortOpt :: ShortOpt -> Parser ()
pendingShortOpt so = Parser $ \s@(State pends rm stop) ->
  let msg = Error (descLocation s)
        [Expected ("pending short option: -" ++ [unShortOpt so])]
      gd s' = Consumed (Ok () s' msg)
      err = Empty (Fail msg)
  in maybe err gd $ do
    guard $ not stop
    (first, rest) <- case pends of
      [] -> mzero
      x:xs -> return (x, xs)
    when (unShortOpt so /= first) mzero
    return $ State rest rm stop

-- | @lookAhead p@ runs parser p. If p succeeds, lookAhead p succeeds
-- without consuming any input. If p fails without consuming any
-- input, so does lookAhead. If p fails and consumes input, lookAhead
-- also fails and consumes input. If this is undesirable, combine with
-- "try".
lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \s ->
  case runParser p s of
    Consumed r -> case r of
      Ok x _ e -> Empty (Ok x s e)
      e -> Consumed e
    e -> e

nextW :: Remaining -> Maybe (String, Remaining)
nextW rm = case rm of
  [] -> Nothing
  x:xs -> Just (x, xs)

-- | Parses only non-pending short options. Fails without consuming
-- any input if:
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
nonPendingShortOpt so = Parser $ \s@(State ps rm stop) ->
  let dsc = [Expected
            $ "non pending short option: -" ++ [unShortOpt so]]
      err = Error (descLocation s) dsc
      errRet = Empty (Fail err)
      gd (ps'', rm'') = Consumed (Ok () (State ps'' rm'' stop) err)
  in maybe errRet gd $ do
    guard $ null ps
    guard $ not stop
    (a, rm') <- nextW rm
    (maybeDash, word) <- case a of
      [] -> mzero
      x:xs -> return (x, xs)
    guard (maybeDash == '-')
    (letter, arg) <- case word of
      [] -> mzero
      x:xs -> return (x, xs)
    guard (letter == unShortOpt so)
    return (arg, rm')


-- | Parses an exact long option. That is, the text of the
-- command-line option must exactly match the text of the
-- option. Returns any argument that is attached to
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
exactLongOpt lo = Parser $ \s@(State ps rm sp) ->
  let msg = Error (descLocation s)
            [Expected ("long option: --" ++ unLongOpt lo)]
      gd (arg, newRm) = Consumed (Ok arg (State ps newRm sp) msg)
      err = Empty (Fail msg)
  in maybe err gd $ do
    guard $ null ps
    guard $ not sp
    (x, rm') <- nextW rm
    (word, afterEq) <- getLongOption x
    guard (word == unLongOpt lo)
    return (afterEq, rm')
    

getLongOption :: String -> Maybe (String, Maybe String)
getLongOption str = do
  guard (str /= "--")
  let (pre, word, afterEq) = splitLongWord str
  guard (pre == "--")
  return (word, afterEq)

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

approxLongOptError :: [LongOpt] -> [Description]
approxLongOptError =
  map (Expected . ("long option: --" ++) . unLongOpt)


-- | Examines the next word. If it matches a LongOpt in the set
-- unambiguously, returns a tuple of the word actually found and the
-- matching word in the set and the accompanying text after the equal
-- sign (if any). If the Set is empty, this parser will always fail.
approxLongOpt ::
  Set LongOpt
  -> Parser (String, LongOpt, Maybe String)
approxLongOpt ts = Parser $ \s@(State ps rm stop) ->
  let err ls = Error (descLocation s) (approxLongOptError ls)
      ert ls = Empty (Fail $ err ls)
      gd (found, opt, arg, rm'') =
        Consumed (Ok (found, opt, arg) (State ps rm'' stop)
                     (err allOpts))
      allOpts = Set.toList ts
  in Ex.switch ert gd $ do
    Ex.assert allOpts $ null ps
    Ex.assert allOpts $ not stop
    (x, rm') <- Ex.fromMaybe allOpts $ nextW rm
    (word, afterEq) <- Ex.fromMaybe allOpts $ getLongOption x
    opt <- Ex.fromMaybe allOpts $ makeLongOpt word
    if Set.member opt ts
      then return (word, opt, afterEq, rm')
      else do
      let p t = word `isPrefixOf` unLongOpt t
          matches = Set.filter p ts
      case Set.toList matches of
        [] -> Ex.throw allOpts
        (m:[]) -> return (word, m, afterEq, rm')
        ls -> Ex.throw ls


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
-- On success, returns the String of the pending short option argument
-- (this String will never be empty).
pendingShortOptArg :: Parser String
pendingShortOptArg = Parser $ \st@(State ps rm sp) ->
  let msg = [Expected "pending short option argument"]
      err = Error (descLocation st) msg
      ert = Empty (Fail err)
      gd str = Consumed (Ok str (State "" rm sp) err)
  in maybe ert gd $ do
     guard $ not sp
     case ps of
      [] -> mzero
      xs -> return xs


-- | Parses a \"stopper\" - that is, a double dash. Changes the internal
-- state of the parser to reflect that a stopper has been seen.
stopper :: Parser ()
stopper = Parser $ \s@(State ps rm sp) ->
  let err = Error (descLocation s)
        [Expected "stopper, \"--\""]
      ert = Empty (Fail err)
      gd rm'' = Consumed (Ok () (State ps rm'' True) err)
  in maybe ert gd $ do
     guard $ not sp
     guard . null $ ps
     (x, rm') <- nextW rm
     guard $ x == "--"
     return rm'


-- | If a stopper has already been seen, change the internal state
-- back to indicating that no stopper has been seen.
resetStopper :: Parser ()
resetStopper = Parser $ \s@(State ps rm _) ->
  Empty (Ok () (State ps rm False) (Error (descLocation s) []))


-- | try p behaves just like p, but if p fails, try p will not consume
-- any input.
try :: Parser a -> Parser a
try a = Parser $ \s ->
  case runParser a s of
    Consumed r -> case r of
      Fail e -> Empty (Fail e)
      o -> Consumed o
    o -> o


-- | Returns the next string on the command line as long as there are
-- no pendings. Succeeds even if a stopper is present. Be careful -
-- this will return the next string even if it looks like an option
-- (that is, it starts with a dash.) Consider whether you should be
-- using nonOptionPosArg instead. However this can be useful when
-- parsing command line options after a stopper.
nextWord :: Parser String
nextWord = Parser $ \s@(State ps rm sp) ->
  let err = Error (descLocation s) [dsc]
      dsc = Expected "next word"
      ert = Empty (Fail err)
      gd (str, rm'') = Consumed $ Ok str (State ps rm'' sp) err
  in maybe ert gd $ do
      guard $ null ps
      nextW rm
      

-- | Parses the next word on the command line, but only if it exactly
-- matches the word given. Otherwise, fails without consuming any
-- input. Also fails without consuming any input if there are pending
-- short options or if a stopper has already been parsed. Does not pay
-- any attention to whether a stopper is present.
nextWordIs :: String -> Parser ()
nextWordIs str = Parser $ \s@(State ps rm sp) ->
  let err = Error (descLocation s) [dsc]
      dsc = Expected $ "next argument \"" ++ str ++ "\""
      ert = Empty $ Fail err
      gd rm'' = Consumed $ Ok () (State ps rm'' sp) err
  in maybe ert gd $ do
      guard $ null ps
      (a, rm') <- nextW rm
      guard (a == str)
      return rm'


-- | If there are pending short options, fails without consuming any input.
--
-- Otherwise, if a stopper has NOT already been parsed, then returns
-- the next word if it is either a single dash or any other word that
-- does not begin with a dash. If the next word does not meet these
-- criteria, fails without consuming any input.
--
-- Otherwise, if a stopper has already been parsed, then returns the
-- next word, regardless of whether it begins with a dash or not.
nonOptionPosArg :: Parser String
nonOptionPosArg = Parser $ \s@(State ps rm sp) ->
  let err = Error (descLocation s) [dsc]
      dsc = Expected "non option positional argument"
      ert = Empty $ Fail err
      gd (str, rm'') = Consumed $ Ok str (State ps rm'' sp) err
  in maybe ert gd $ do
    guard $ null ps
    (x, rm') <- nextW rm
    result <- if sp
              then return x
              else case x of
                [] -> return x
                '-':[] -> return "-"
                f:_ -> if f == '-' then mzero else return x
    return (result, rm')


-- | Succeeds if there is no more input left.
end :: Parser ()
end = Parser $ \s@(State ps rm _) ->
  let err = Error (descLocation s) [dsc]
      dsc = Expected "end of input"
      ert = Empty $ Fail err
      gd = Empty $ Ok () s err
  in if null ps && null rm then gd else ert


-- | Examines the possible words in Set. If there are no pendings,
-- then get the next word and see if it matches one of the words in
-- Set. If so, returns the word actually parsed and the matching word
-- from Set. If there is no match, fails without consuming any
-- input. Pays no attention to whether a stopper has been seen.
matchApproxWord :: Set String -> Parser (String, String)
matchApproxWord set = Parser $ \s@(State ps rm sp) ->
  let err = Error (descLocation s) . lsDsc
      lsDsc = map (Expected . ("next word: " ++))
      ert = Empty . Fail . err
      gd (act, mtch, rm'') =
        Consumed $ Ok (act, mtch) (State ps rm'' sp) (err allWords)
      allWords = Set.toList set
  in Ex.switch ert gd $ do
      Ex.assert allWords $ null ps
      (x, rm') <- Ex.fromMaybe allWords $ nextW rm
      let matches = Set.filter p set
          p t = x `isPrefixOf` t
      case Set.toList matches of
        [] -> Ex.throw allWords
        r:[] -> return (x, r, rm')
        xs -> Ex.throw xs
      
-- | @manyTill p end@ runs parser p zero or more times until parser
-- @end@ succeeds. If @end@ succeeds and consumes input, that input is
-- also consumed. in the result of @manyTill@. If that is a problem,
-- wrap it in @lookAhead@. Also, if @end@ fails and consumes input,
-- @manyTill@ fails and consumes input. If that is a problem, wrap
-- @end@ in @try@.
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p e = do
  maybeEnd <- optional e
  case maybeEnd of
    Just _ -> return []
    Nothing -> do
      a <- crashOnEmptyOk "manyTill" p
      rs <- manyTill p e
      return $ a:rs

