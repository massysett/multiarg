{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Parser primitives. These are the only functions that have access
-- to the internals of the parser.
module System.Console.MultiArg.Prim (
  -- * Parser types
  ParserSE,
  ParserE,
  Parser,
  
  -- * Running a parser
  runParserSE,
  runParser,
  
  -- * Higher-level parser combinators
  zero,
  (<|>),
  (<?>),
  try,
  many,
  manyTill,
  
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
  getSt,
  putSt,
  modifySt ) where

import qualified System.Console.MultiArg.Error as E
import System.Console.MultiArg.Error ( unexpected )
import System.Console.MultiArg.Option
  (ShortOpt,
    unShortOpt,
    LongOpt,
    unLongOpt )
import System.Console.MultiArg.TextNonEmpty
  ( TextNonEmpty ( TextNonEmpty ) )
import Control.Applicative ( Applicative )
import Control.Monad.Exception.Synchronous
  ( ExceptionalT (ExceptionalT), runExceptionalT, throwT,
    Exceptional(Success, Exception) )
import Control.Monad.Trans.State.Lazy
  ( State, state, get, runState, put,
    modify, runStateT )
import Data.Functor.Identity ( runIdentity )
import Data.Text ( Text, pack, isPrefixOf, cons )
import qualified Data.Text as X
import qualified Data.Set as Set
import Data.Set ( Set )
import Control.Monad ( when, liftM )
import Control.Monad.Trans.Class ( lift )
import Test.QuickCheck ( Arbitrary ( arbitrary ),
                         suchThat,
                         CoArbitrary ( coarbitrary ),
                         (><), choose )
import System.Console.MultiArg.QuickCheckHelpers ( WText(WText) )
import Test.QuickCheck.Gen ( oneof )
import Text.Printf ( printf )
import Data.Maybe ( isNothing, isJust, fromJust, fromMaybe )
import Data.List ( find )

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

-- | For testing
newtype ParseStNoUser =
  ParseStNoUser { unParseStNoUser :: ParseSt () }
  deriving Show

instance Arbitrary ParseStNoUser where
  arbitrary = do
    ps <- arbitrary
    rem <- liftM (map pack) arbitrary
    ss <- arbitrary
    c <- arbitrary
    return . ParseStNoUser $ ParseSt ps rem ss () c

instance CoArbitrary ParseStNoUser where
  coarbitrary (ParseStNoUser (ParseSt ps r ss u c)) =
    coarbitrary ps
    >< coarbitrary (map WText r)
    >< coarbitrary ss
    >< coarbitrary u
    >< coarbitrary c

defaultState :: s -> [Text] -> ParseSt s
defaultState user ts = ParseSt { pendingShort = Nothing
                               , remaining = ts
                               , sawStopper = False
                               , userState = user
                               , counter = 0 }

-- | The parser type. Inside this is an ExceptionalT transformer on
-- top of a State monad. @ParserSE s e a@ is a parser with user state
-- s, error type e, and return type a.
newtype ParserSE s e a =
  ParserSE { unParserSE :: ExceptionalT e (State (ParseSt s)) a }
  deriving (Monad, Functor, Applicative)

newtype TestParserSE =
  TestParserSE { unTestParserSE :: ParserSE () E.SimpleError Int }
  
instance Show TestParserSE where
  show _ = "<TestParserSE>"

instance Arbitrary TestParserSE where
  arbitrary = do
    f <- arbitrary
    let f' st = (unwrappedRes, unwrappedSt) where
          unwrappedRes = unWExceptional wrappedRes
          unwrappedSt = unParseStNoUser wrappedSt
          (wrappedRes, wrappedSt) = f (ParseStNoUser st)
    return (TestParserSE (ParserSE (ExceptionalT (state f'))))

newtype WExceptional =
  WExceptional { unWExceptional :: Exceptional E.SimpleError Int }
                       deriving Show

instance Arbitrary WExceptional where
  arbitrary = do
    i <- choose (0, 1 :: Int)
    r <- case i of
      0 -> liftM Exception arbitrary
      1 -> liftM Success arbitrary
      _ -> error "should never happen"
    return . WExceptional $ r

-- | A parser without user state (more precisely, its user state is
-- the empty tuple) but with a parameterizable error type. @ParserE e
-- a@ is a parser user state @()@ and error type e and return type a.
type ParserE e a = ParserSE ()

-- | A parser without user state (more precisely, its user state is
-- the empty tuple) and with an error type SimpleError. @Parser a@ is
-- a parser with user state @()@ and error type SimpleError and return
-- type a.
type Parser a = ParserSE () E.SimpleError a

-- | Runs a parser with user state s. Returns the parse result and the
-- new user state.
runParserSE :: s
               -> [Text]
               -> ParserSE s e a
               -> Exceptional e (a, s)
runParserSE user ts (ParserSE c) = let
  s = defaultState user ts
  in case flip runState s . runExceptionalT $ c of
    ((Success g), s') -> Success (g, userState s')
    ((Exception e), _) -> Exception e

-- | Runs a parser without user state.
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
    (Success g) -> do
      lift $ put s'
      return g
    (Exception e) ->
      if noConsumed s s'
      then do
        let (a2, s'') = flip runState s . runExceptionalT $ r
        lift $ put s''
        case a2 of
          (Success g) -> return g
          (Exception e') -> throwT e'
      else do
        lift $ put s'
        throwT e

-- The old code for <|> looked like the following commented out
-- code. This code is broken. It runs the left parser more than once
-- if it succeeds - it runs it once to see if it succeeds, then
-- returns a parser that runs the parser again. This function
-- typechecks OK but it can lead to infinite recursion bugs. It looks
-- simpler but it is broken; do not use.
{-
(<|>) (ParserE l) (ParserE r) = ParserE $ do
  s <- lift get
  let (a1, s') = flip runState s . runExceptionalT $ l
  case a1 of
    (Success _) -> l
    (Exception _) ->
      if noConsumed s s' then r else l
-}
infixr 1 <|>

prop_choose :: (TestParserSE, TestParserSE, ParseStNoUser) -> Bool
prop_choose (tp1, tp2, stWrapped) = ex == act where
  p1 = unTestParserSE $ tp1
  p2 = unTestParserSE $ tp2
  st = unParseStNoUser stWrapped
  act = ((runState . runExceptionalT) (unParserSE (p1 <|> p2))) st
  fn = case result of
    (Success _) -> p1
    (Exception _) -> if noConsumed st st' then p2 else p1
  (result, st') = (runState . runExceptionalT $ (unParserSE p1)) st
  ex = (runState. runExceptionalT $ (unParserSE fn)) st

-- | noConsumed old new sees if any input was consumed between when
-- old was the state and when new was the state.
noConsumed :: ParseSt st -> ParseSt st -> Bool
noConsumed old new = counter old >= counter new

prop_noConsumed :: (ParseStNoUser, ParseStNoUser) -> Bool
prop_noConsumed p = ex == act where
  ((ParseStNoUser old), (ParseStNoUser new)) = p
  act = noConsumed old new
  ex = if counter new <= counter old then True else False

-- | Runs the parser given. If it succeeds, then returns the result of
-- the parser. If it fails and consumes input, returns the result of
-- the parser. If it fails without consuming any input, then changes
-- the error using the function given.
(<?>) :: ParserSE s e a -> e -> ParserSE s e a
(<?>) (ParserSE l) e = ParserSE $ do
  s <- lift get
  let (a1, s') = flip runState s . runExceptionalT $ l
  case a1 of
    (Success _) -> l
    (Exception e') ->
      if noConsumed s s' then throwT e else l

infix 0 <?>

-- | Installs a new state, and increments the count of how many times
-- the state has been modified.
increment :: ParseSt s -> ExceptionalT e (State (ParseSt s)) ()
increment s = lift (put s) >>
  lift (modify (\st -> st { counter = succ . counter $ st } ))

-- | Parses only pending short options. Fails without consuming any
-- input if there has already been a stopper or if there are no
-- pending short options. Fails without consuming any input if there
-- is a pending short option, but it does not match the short option
-- given. Succeeds and consumes a pending short option if it matches
-- the short option given; returns the short option parsed.
pendingShortOpt :: (E.Error e) => ShortOpt -> ParserSE s e ShortOpt
pendingShortOpt so = ParserSE $ do
  s <- lift get
  let err saw = throwT (unexpected (E.ExpPendingShortOpt so) saw)
  when (sawStopper s) (err E.SawAlreadyStopper)
  (TextNonEmpty first rest) <-
    maybe (err E.SawNoPendingShorts) return (pendingShort s)
  when (unShortOpt so /= first) (err $ E.SawWrongPendingShort first)
  increment s { pendingShort = toTextNonEmpty rest }
  return so

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
nonPendingShortOpt :: (E.Error e) => ShortOpt -> ParserSE s e ShortOpt
nonPendingShortOpt so = ParserSE $ do
  let err saw = throwT (unexpected (E.ExpNonPendingShortOpt so) saw)
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
  increment s { pendingShort = toTextNonEmpty arg
              , remaining = as }
  return so

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
  increment s { remaining = xs }
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
      increment s { remaining = xs }
      return (word, m, afterEq)
    _ -> err (E.SawMultipleMatches matches word)

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
pendingShortOptArg :: (E.Error e) => ParserSE s e Text
pendingShortOptArg = ParserSE $ do
  let err saw = throwT $ unexpected E.ExpPendingShortArg saw
  s <- lift get
  when (sawStopper s) (err E.SawAlreadyStopper)
  let f (TextNonEmpty c t) = return (c `cons` t)
  a <- maybe (err E.SawNoPendingShortArg) f (pendingShort s)
  increment s { pendingShort = Nothing }
  return a

-- | Parses a "stopper" - that is, a double dash. Changes the internal
-- state of the parser to reflect that a stopper has been seen.
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
  increment s { sawStopper = True
               , remaining = xs }

{-
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
-}

-- | try p behaves just like p, but if p fails, try p will not consume
-- any input.
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
  increment s { remaining = xs }
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
  increment s { remaining = xs }
  return result

newtype ParseStNonOptionPosArg =
  ParseStNonOptionPosArg { unParseStNonOptionPosArg :: ParseSt () }
  deriving Show

instance Arbitrary ParseStNonOptionPosArg where
  arbitrary = do
    ps <- oneof [ return Nothing, arbitrary ]
    let textDash = do
          let first = '-'
          rest <- arbitrary
          return $ pack (first : rest)
    let remWithDash = do
          first <- textDash
          rest <- liftM (map pack) arbitrary
          return (first : rest)
    rem <- oneof [ remWithDash, liftM (map pack) arbitrary ]
    ss <- arbitrary
    counter <- arbitrary
    return
      . ParseStNonOptionPosArg
      $ ParseSt ps rem ss () counter

prop_nonOptionPosArg :: ParseStNonOptionPosArg -> Bool
prop_nonOptionPosArg (ParseStNonOptionPosArg st) = ex == act where
  act = unwrapParser nonOptionPosArg st
        :: (Exceptional E.SimpleError Text, ParseSt ())
  ex = (err, st') where
    st' = case err of
      (Success _) -> st { remaining = tail (remaining st)
                        , counter = succ (counter st) }
      (Exception _) -> st
    err = switch good [stillPending, noArgs, noStop, leadingDash]
    uxp e = Exception (unexpected E.ExpNonOptionPosArg e)
    good = (Success (head (remaining st)))
    stillPending = (test, res) where
      test = isJust (pendingShort st)
      res = uxp (E.SawStillPendingShorts (fromJust (pendingShort st)))
    noArgs = (test, res) where
      test = null (remaining st)
      res = uxp (E.SawNoArgsLeft)
    noStop = (test, res) where
      test = sawStopper st
      res = (Success (head (remaining st)))
    leadingDash = (test, res) where
      test = (fst . fromJust . textHead . head . remaining $ st)
             == '-'
      res = uxp (E.SawLeadingDashArg (head . remaining $ st))
      

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
-- Be particularly careful to get the order of the arguments
-- correct. Applying this function to reversed arguments will yield
-- bugs that are very difficult to diagnose.
manyTill :: ParserSE s e a
            -> ParserSE s e end
            -> ParserSE s e [a]
manyTill (ParserSE r) (ParserSE f) = ParserSE $ do
  s <- lift get
  let unwrap fn st = runIdentity
                     . flip runStateT st
                     . runExceptionalT
                     $ fn
      fr = unwrap r
      ff = unwrap f
      till = parseTill s fr ff
  lift (put (lastSt till))
  maybe (return . goods $ till) throwT (lastFailure till)

data Till a s e =
  Till { goods :: [a]
       , lastSt :: ParseSt s
       , lastFailure :: Maybe e }

parseTill :: ParseSt s
             -> (ParseSt s -> (Exceptional e a, ParseSt s))
             -> (ParseSt s -> (Exceptional e b, ParseSt s))
             -> Till a s e
parseTill s fr ff = case ff s of
  (Success _, _) -> Till [] s Nothing
  (Exception _, _) -> case fr s of
    (Exception re, st'') -> Till [] st'' (Just re)
    (Success a, st'') -> let
      t = parseTill st'' fr ff
      in if counter st'' == counter s
         then error "parseTill applied to parser that takes empty list"
         else Till (a : goods t) (lastSt t) (lastFailure t)

-- | many p runs parser p zero or more times and returns all the
-- results. This proceeds like this: parser p is run and, if it
-- succeeds, the result is saved and parser p is run
-- again. Repeat. Eventually this will have to fail. If the last run
-- of parser p fails without consuming any input, then many p runs
-- successfully. The state of the parser is updated to reflect the
-- successful runs of p. If the last run of parser p fails but it
-- consumed input, then many p fails. The state of the parser is
-- updated to reflect the state up to and including the run that
-- partially consumed input. The parser is left in a failed state.
--
-- This semantic can come in handy. For example you might run a parser
-- multiple times that parses an option and arguments to the
-- option. If the arguments fail to parse, then many will fail.
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

parseRepeat :: ParseSt s
          -> (ParseSt s -> (Exceptional e a, ParseSt s))
          -> ([a], ParseSt s, e, ParseSt s)
parseRepeat st1 f = case f st1 of
  (Success a, st') -> let
    (ls, finalGoodSt, failure, finalBadSt) = parseRepeat st' f
    in if noConsumed st1 st'
       then error "parseRepeat applied to parser that takes empty list"
       else (a : ls, finalGoodSt, failure, finalBadSt)
  (Exception e, st') -> ([], st1, e, st')

-- | Succeeds if there is no more input left.
end :: (E.Error e) => ParserSE s e ()
end = ParserSE $ do
  let err saw = throwT $ unexpected E.ExpEnd saw
  s <- lift get
  maybe (return ()) (err . E.SawStillPendingShorts) (pendingShort s)
  when (not . null . remaining $ s) (err E.SawMoreInput)
  return ()

unwrapParser :: ParserSE s e a
                -> ParseSt s
                -> (Exceptional e a, ParseSt s)
unwrapParser p s = (runState
                   . runExceptionalT
                   . unParserSE
                   $ p) s

prop_end :: ParseStNoUser -> Bool
prop_end (ParseStNoUser st) = exp == actual where
  actual = unwrapParser end st
           :: (Exceptional E.SimpleError (), ParseSt ())
  exp = if isJust (pendingShort st)
        then (Exception (unexpected E.ExpEnd
                         (E.SawStillPendingShorts
                          (fromJust . pendingShort $ st)))
              , st)
        else if not . null . remaining $ st
             then (Exception (unexpected E.ExpEnd
                                 (E.SawMoreInput)), st)
             else (Success (), st)

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

switch :: r -> [(Bool, r)] -> r
switch i ls = case find fst ls of
  (Just (_, r)) -> r
  Nothing -> i
