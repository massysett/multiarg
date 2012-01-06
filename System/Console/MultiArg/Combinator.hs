-- | Combinators that are useful for building command-line
-- parsers. These build off the functions in
-- "System.Console.MultiArg.Prim". Unlike those functions, these
-- functions have no access to the internals of the parser. TODO
-- rename these from Single, Double, etc to OneArg, TwoArg, etc.
module System.Console.MultiArg.Combinator (
  -- * Parser combinators
  option,
  optionMaybe,
  choice,
  
  -- * Short options
  shortNoArg,
  shortOptionalArg,
  shortSingleArg,
  shortDoubleArg,
  shortVariableArg,

  -- * Long options
  nonGNUexactLongOpt,
  matchApproxLongOpt,
  matchNonGNUApproxLongOpt,
  longNoArg,
  longOptionalArg,
  longSingleArg,
  longDoubleArg,
  longVariableArg,
  
  -- * Other words
  matchApproxWord ) where
  
  

import Data.Text ( Text, isPrefixOf )
import Data.Set ( Set )
import qualified Data.Set as Set
import Control.Monad ( liftM )

import System.Console.MultiArg.Prim
  ( (<|>), ParserSE, zero, try, approxLongOpt,
    nextArg, pendingShortOptArg, nonOptionPosArg,
    pendingShortOpt, nonPendingShortOpt, many,
    exactLongOpt, nextArg, (<?>))
import System.Console.MultiArg.Option
  ( LongOpt, ShortOpt )
import qualified System.Console.MultiArg.Error as E
import System.Console.MultiArg.Error
  ( Error, unexpected )

-- | @option x p@ runs parser p. If p fails without consuming any
-- input, returns x. Otherwise, returns p.
option :: a -> ParserSE s e a -> ParserSE s e a
option x p = p <|> return x

-- | @optionMaybe p@ runs parser p. If p fails without returning any
-- input, returns Nothing. If p succeeds, returns the result of p
-- wrapped in a Just. If p fails but consumes input, optionMaybe
-- fails.
optionMaybe :: ParserSE s e a -> ParserSE s e (Maybe a)
optionMaybe p = option Nothing (liftM Just p)

-- | @choice e ps@ runs parsers from ps in order. choice returns the
-- first parser that either succeeds or fails while consuming
-- input. For each parser, if it fails without consuming any input,
-- the next parser is tried. If all the parsers fail without consuming
-- any input, the last parser will be returned. If ps is empty,
-- returns e. (TODO just have this take some sort of non-empty list;
-- that would make more sense.)
choice :: e -> [ParserSE s e a] -> ParserSE s e a
choice e ls = foldl (<|>) (zero e) ls

-- | Parses only a non-GNU style long option (that is, one that does
-- not take option arguments by attaching them with an equal sign,
-- such as @--lines=20@).
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

-- | Takes a long option and a set of long options. If the next word
-- on the command line unambiguously starts with the name of the long
-- option given, returns the actual text found on the command line,
-- the long option, and the text of any GNU-style option
-- argument. Make sure that the long option you are looking for is
-- both the first argument and that it is included in the set;
-- otherwise this parser will always fail.
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

-- | Like matchApproxLongOpt but only parses non-GNU-style long
-- options.
matchNonGNUApproxLongOpt :: (Error e)
                            => LongOpt
                            -> Set LongOpt
                            -> ParserSE s e (Text, LongOpt)
matchNonGNUApproxLongOpt l s = try $ do
  (t, lo, arg) <- matchApproxLongOpt l s
  let err b = zero (unexpected (E.ExpNonGNUMatchingApproxLong l s)
                    (E.SawMatchingApproxLongWithArg b))
  maybe (return (t, lo)) err arg

-- | Examines the possible words in Set. If there are no pendings,
-- then get the next word and see if it matches one of the words in
-- Set. If so, returns the word actually parsed and the matching word
-- from Set. If there is no match, fails without consuming any input.
matchApproxWord :: (Error e)
                   => Set Text
                   -> ParserSE s e (Text, Text)
matchApproxWord s = try $ do
  a <- nextArg
  let p t = a `isPrefixOf` t
      matches = Set.filter p s
      err saw = zero (unexpected (E.ExpApproxWord s) saw)
  case Set.toList matches of
    [] -> err (E.SawNoMatches a)
    (x:[]) -> return (a, x)
    _ -> err (E.SawMultipleApproxMatches matches a)

-- | Parses short options that do not take any argument. (It is
-- however okay for the short option to be combined with other short
-- options in the same word.)
shortNoArg :: (Error e)
            => ShortOpt
            -> ParserSE s e ShortOpt
shortNoArg s = pendingShortOpt s <|> nonPendingShortOpt s

-- | Parses short options that take an optional argument. The argument
-- can be combined in the same word with the short option (@-c42@) or
-- can be in the ext word (@-c 42@).
shortOptionalArg :: (Error e)
                 => ShortOpt
                 -> ParserSE s e (ShortOpt, Maybe Text)
shortOptionalArg s = do
  so <- shortNoArg s
  a <- optionMaybe (pendingShortOptArg <|> nonOptionPosArg)
  return (so, a)

-- | Parses short options that take a required argument.  The argument
-- can be combined in the same word with the short option (@-c42@) or
-- can be in the ext word (@-c 42@).
shortSingleArg :: (Error e) =>
               ShortOpt
               -> ParserSE s e (ShortOpt, Text)
shortSingleArg s = do
  so <- shortNoArg s
  a <- pendingShortOptArg <|> nextArg
  return (so, a)

-- | Parses short options that take two required arguments. The first
-- argument can be combined in the same word with the short option
-- (@-c42@) or can be in the ext word (@-c 42@). The next argument
-- will have to be in a separate word.
shortDoubleArg :: (Error e)
               => ShortOpt
               -> ParserSE s e (ShortOpt, Text, Text)
shortDoubleArg s = do
  (so, a1) <- shortSingleArg s
  a2 <- nextArg
  return (so, a1, a2)

-- | Parses short options that take a variable number of
-- arguments. This will keep on parsing option arguments until it
-- encounters one that does not "look like" an option--that is, until
-- it encounters one that begins with a dash. Therefore, the only way
-- to terminate a variable argument option if it is the last option is
-- with a stopper. The first argument can be combined in the same word
-- with the short option (@-c42@) or can be in the ext word (@-c
-- 42@). Subsequent arguments will have to be in separate words.
shortVariableArg :: (Error e)
                 => ShortOpt
                 -> ParserSE s e (ShortOpt, [Text])
shortVariableArg s = do
  so <- shortNoArg s
  firstArg <- optionMaybe pendingShortOptArg
  rest <- many nonOptionPosArg
  let result = maybe rest ( : rest ) firstArg
  return (so, result)

-- | Parses long options that do not take any argument.
longNoArg :: (Error e)
           => LongOpt
           -> ParserSE s e LongOpt
longNoArg = nonGNUexactLongOpt

-- | Parses long options that take a single, optional argument. The
-- single argument can be given GNU-style (@--lines=20@) or non-GNU
-- style in separate words (@lines 20@).
longOptionalArg :: (Error e)
                   => LongOpt
                   -> ParserSE s e (LongOpt, Maybe Text)
longOptionalArg = exactLongOpt

-- | Parses long options that take a single, required argument. The
-- single argument can be given GNU-style (@--lines=20@) or non-GNU
-- style in separate words (@lines 20@).
longSingleArg :: (Error e)
                 => LongOpt
                 -> ParserSE s e (LongOpt, Text)
longSingleArg l = do
  (lo, mt) <- longOptionalArg l
  case mt of
    (Just t) -> return (lo, t)
    Nothing -> do
      a <- nextArg <?> const (E.unexpected E.ExpLongOptArg E.SawNoArgsLeft)
      return (l, a)

-- | Parses long options that take a double, required argument. The
-- first argument can be given GNU-style (@--lines=20@) or non-GNU
-- style in separate words (@lines 20@). The second argument will have
-- to be in a separate word.
longDoubleArg :: (Error e)
                 => LongOpt
                 -> ParserSE s e (LongOpt, Text, Text)
longDoubleArg l = do
  (lo, mt) <- longOptionalArg l
  case mt of
    (Just t) -> do
      a2 <- nextArg
      return (lo, t, a2)
    Nothing -> do
      a1 <- nextArg
      a2 <- nextArg
      return (lo, a1, a2)

-- | Parses long options that take a variable number of
-- arguments. This will keep on parsing option arguments until it
-- encounters one that does not "look like" an option--that is, until
-- it encounters one that begins with a dash. Therefore, the only way
-- to terminate a variable argument option if it is the last option is
-- with a stopper. The first argument can be combined in the same word
-- with the short option (@--lines=20@) or can be in the ext word
-- (@--lines 42@). Subsequent arguments will have to be in separate
-- words.
longVariableArg :: (Error e)
                   => LongOpt
                   -> ParserSE s e (LongOpt, [Text])
longVariableArg l = do
  (lo, mt) <- longOptionalArg l
  rest <- many nonOptionPosArg
  return (lo, maybe rest (:rest) mt)
