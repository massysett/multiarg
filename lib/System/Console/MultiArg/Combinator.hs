-- | Combinators that are useful for building command-line
-- parsers. These build off the functions in
-- "System.Console.MultiArg.Prim". Unlike those functions, these
-- functions have no access to the internals of the parser.
module System.Console.MultiArg.Combinator (
  -- * Parser combinators
  option,
  optionMaybe,
  notFollowedBy,
  
  -- * Short options
  shortNoArg,
  shortOptionalArg,
  shortOneArg,
  shortTwoArg,
  shortVariableArg,

  -- * Long options
  nonGNUexactLongOpt,
  matchApproxLongOpt,
  matchNonGNUApproxLongOpt,
  longNoArg,
  longOptionalArg,
  longOneArg,
  longTwoArg,
  longVariableArg,
  
  -- * Mixed options
  mixedNoArg,
  mixedOptionalArg,
  mixedOneArg,
  mixedTwoArg,
  mixedVariableArg,
  
  -- * Other words
  matchApproxWord ) where
  
import Data.Text ( Text, isPrefixOf )
import Data.Set ( Set )
import qualified Data.Set as Set
import Control.Monad ( liftM )

import System.Console.MultiArg.Prim
  ( ParserT, throw, try, approxLongOpt,
    nextArg, pendingShortOptArg, nonOptionPosArg,
    pendingShortOpt, nonPendingShortOpt,
    exactLongOpt, nextArg, (<?>))
import System.Console.MultiArg.Option
  ( LongOpt, ShortOpt )
import qualified System.Console.MultiArg.Error as E
import System.Console.MultiArg.Error
  ( Error, parseErr )
import Control.Applicative ((<|>), many)
import Control.Monad ( void )
import Data.Monoid ( mconcat )

-- | @option x p@ runs parser p. If p fails without consuming any
-- input, returns x. Otherwise, returns p.
option :: (Error e, Monad m) =>
          a
          -> ParserT s e m a
          -> ParserT s e m a
option x p = p <|> return x

-- | @optionMaybe p@ runs parser p. If p fails without returning any
-- input, returns Nothing. If p succeeds, returns the result of p
-- wrapped in a Just. If p fails but consumes input, optionMaybe
-- fails.
optionMaybe :: (Error e, Monad m)
               => ParserT s e m a
               -> ParserT s e m (Maybe a)
optionMaybe p = option Nothing (liftM Just p)

-- | @notFollowedBy p@ succeeds only if parser p fails. If p fails,
-- notFollowedBy succeeds without consuming any input. If p succeeds
-- and consumes input, notFollowedBy fails and consumes input. If p
-- succeeds and does not consume any input, notFollowedBy fails and
-- does not consume any input.
notFollowedBy :: (Error e, Monad m)
                 => ParserT s e m a
                 -> ParserT s e m ()
notFollowedBy p =
  void $ ((try p >> throw (E.parseErr E.ExpNotFollowedBy E.SawFollowedBy))
          <|> return ())


-- | Parses only a non-GNU style long option (that is, one that does
-- not take option arguments by attaching them with an equal sign,
-- such as @--lines=20@).
nonGNUexactLongOpt :: (Error e, Monad m)
                      => LongOpt
                      -> ParserT s e m LongOpt
nonGNUexactLongOpt l = try $ do
  (lo, maybeArg) <- exactLongOpt l
  case maybeArg of
    Nothing -> return lo
    (Just t) ->
      throw (parseErr (E.ExpNonGNUExactLong l)
            (E.SawGNULongOptArg t))

-- | Takes a long option and a set of long options. If the next word
-- on the command line unambiguously starts with the name of the long
-- option given, returns the actual text found on the command line,
-- the long option, and the text of any GNU-style option
-- argument. Make sure that the long option you are looking for is
-- both the first argument and that it is included in the set;
-- otherwise this parser will always fail.
matchApproxLongOpt :: (Error e, Monad m)
                      => LongOpt
                      -> Set LongOpt
                      -> ParserT s e m (Text, LongOpt, Maybe Text)
matchApproxLongOpt l s = try $ do
  a@(t, lo, _) <- approxLongOpt s
  if lo == l
    then return a
    else throw (parseErr (E.ExpMatchingApproxLong l s)
               (E.SawNotMatchingApproxLong t lo))

-- | Like matchApproxLongOpt but only parses non-GNU-style long
-- options.
matchNonGNUApproxLongOpt :: (Error e, Monad m)
                            => LongOpt
                            -> Set LongOpt
                            -> ParserT s e m (Text, LongOpt)
matchNonGNUApproxLongOpt l s = try $ do
  (t, lo, arg) <- matchApproxLongOpt l s
  let err b = throw (parseErr (E.ExpNonGNUMatchingApproxLong l s)
                    (E.SawMatchingApproxLongWithArg b))
  maybe (return (t, lo)) err arg

-- | Examines the possible words in Set. If there are no pendings,
-- then get the next word and see if it matches one of the words in
-- Set. If so, returns the word actually parsed and the matching word
-- from Set. If there is no match, fails without consuming any input.
matchApproxWord :: (Error e, Monad m)
                   => Set Text
                   -> ParserT s e m (Text, Text)
matchApproxWord s = try $ do
  a <- nextArg
  let p t = a `isPrefixOf` t
      matches = Set.filter p s
      err saw = throw (parseErr (E.ExpApproxWord s) saw)
  case Set.toList matches of
    [] -> err (E.SawNoMatches a)
    (x:[]) -> return (a, x)
    _ -> err (E.SawMultipleApproxMatches matches a)

-- | Parses short options that do not take any argument. (It is
-- however okay for the short option to be combined with other short
-- options in the same word.)
shortNoArg :: (Error e, Monad m)
            => ShortOpt
            -> ParserT s e m ShortOpt
shortNoArg s = pendingShortOpt s <|> nonPendingShortOpt s

-- | Parses short options that take an optional argument. The argument
-- can be combined in the same word with the short option (@-c42@) or
-- can be in the ext word (@-c 42@).
shortOptionalArg :: (Error e, Monad m)
                 => ShortOpt
                 -> ParserT s e m (ShortOpt, Maybe Text)
shortOptionalArg s = do
  so <- shortNoArg s
  a <- optionMaybe (pendingShortOptArg <|> nonOptionPosArg)
  return (so, a)

-- | Parses short options that take a required argument.  The argument
-- can be combined in the same word with the short option (@-c42@) or
-- can be in the ext word (@-c 42@).
shortOneArg :: (Error e, Monad m) =>
               ShortOpt
               -> ParserT s e m (ShortOpt, Text)
shortOneArg s = do
  so <- shortNoArg s
  a <- pendingShortOptArg <|> nextArg
  return (so, a)

-- | Parses short options that take two required arguments. The first
-- argument can be combined in the same word with the short option
-- (@-c42@) or can be in the ext word (@-c 42@). The next argument
-- will have to be in a separate word.
shortTwoArg :: (Error e, Monad m)
               => ShortOpt
               -> ParserT s e m (ShortOpt, Text, Text)
shortTwoArg s = do
  (so, a1) <- shortOneArg s
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
shortVariableArg :: (Error e, Monad m)
                 => ShortOpt
                 -> ParserT s e m (ShortOpt, [Text])
shortVariableArg s = do
  so <- shortNoArg s
  firstArg <- optionMaybe pendingShortOptArg
  rest <- many nonOptionPosArg
  let result = maybe rest ( : rest ) firstArg
  return (so, result)

-- | Parses long options that do not take any argument.
longNoArg :: (Error e, Monad m)
           => LongOpt
           -> ParserT s e m LongOpt
longNoArg = nonGNUexactLongOpt

-- | Parses long options that take a single, optional argument. The
-- single argument can be given GNU-style (@--lines=20@) or non-GNU
-- style in separate words (@lines 20@).
longOptionalArg :: (Error e, Monad m)
                   => LongOpt
                   -> ParserT s e m (LongOpt, Maybe Text)
longOptionalArg = exactLongOpt

-- | Parses long options that take a single, required argument. The
-- single argument can be given GNU-style (@--lines=20@) or non-GNU
-- style in separate words (@lines 20@).
longOneArg :: (Error e, Monad m)
                 => LongOpt
                 -> ParserT s e m (LongOpt, Text)
longOneArg l = do
  (lo, mt) <- longOptionalArg l
  case mt of
    (Just t) -> return (lo, t)
    Nothing -> do
      a <- nextArg <?> E.parseErr E.ExpLongOptArg E.SawNoArgsLeft
      return (l, a)

-- | Parses long options that take a double, required argument. The
-- first argument can be given GNU-style (@--lines=20@) or non-GNU
-- style in separate words (@lines 20@). The second argument will have
-- to be in a separate word.
longTwoArg :: (Error e, Monad m)
                 => LongOpt
                 -> ParserT s e m (LongOpt, Text, Text)
longTwoArg l = do
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
longVariableArg :: (Error e, Monad m)
                   => LongOpt
                   -> ParserT s e m (LongOpt, [Text])
longVariableArg l = do
  (lo, mt) <- longOptionalArg l
  rest <- many nonOptionPosArg
  return (lo, maybe rest (:rest) mt)

-- | Parses at least one long option and a variable number of short
-- and long options that take no arguments.
mixedNoArg :: (Error e, Monad m)
              => LongOpt
              -> [LongOpt]
              -> [ShortOpt]
              -> ParserT s e m (Either ShortOpt LongOpt)
mixedNoArg l ls ss = mconcat ([f] ++ longs ++ shorts) where
  toLong lo = do
    r <- longNoArg lo
    return $ Right r
  toShort so = do
    s <- shortNoArg so
    return $ Left s
  f = toLong l
  longs = map toLong ls
  shorts = map toShort ss

-- | Parses at least one long option and a variable number of short
-- and long options that take an optional argument.
mixedOptionalArg ::
  (Error e, Monad m)
  => LongOpt
  -> [LongOpt]
  -> [ShortOpt]
  -> ParserT s e m ((Either ShortOpt LongOpt), Maybe Text)
mixedOptionalArg l ls ss = mconcat ([f] ++ longs ++ shorts) where
  toLong lo = do
    (o, a) <- longOptionalArg lo
    return $ (Right o, a)
  toShort so = do
    (o, a) <- shortOptionalArg so
    return $ (Left o, a)
  f = toLong l
  longs = map toLong ls
  shorts = map toShort ss

-- | Parses at least one long option and additional long and short
-- options that take one argument.
mixedOneArg ::
  (Error e, Monad m)
  => LongOpt
  -> [LongOpt]
  -> [ShortOpt]
  -> ParserT s e m ((Either ShortOpt LongOpt), Text)
mixedOneArg l ls ss = mconcat ([f] ++ longs ++ shorts) where
  toLong lo = do
    (o, a) <- longOneArg lo
    return (Right o, a)
  toShort lo = do
    (o, a) <- shortOneArg lo
    return (Left o, a)
  f = toLong l
  longs = map toLong ls
  shorts = map toShort ss

-- | Parses at least one long option and additonal long and short
-- options that take two arguments.
mixedTwoArg ::
  (Error e, Monad m)
  => LongOpt
  -> [LongOpt]
  -> [ShortOpt]
  -> ParserT s e m ((Either ShortOpt LongOpt), Text, Text)
mixedTwoArg l ls ss = mconcat ([f] ++ longs ++ shorts) where
  toLong lo = do
    (o, a1, a2) <- longTwoArg lo
    return (Right o, a1, a2)
  toShort lo = do
    (o, a1, a2) <- shortTwoArg lo
    return (Left o, a1, a2)
  f = toLong l
  longs = map toLong ls
  shorts = map toShort ss

-- | Parses at least one long option and additional long and short
-- options that take a variable number of arguments.
mixedVariableArg ::
  (Error e, Monad m)
  => LongOpt
  -> [LongOpt]
  -> [ShortOpt]
  -> ParserT s e m ((Either ShortOpt LongOpt), [Text])
mixedVariableArg l ls ss = mconcat ([f] ++ longs ++ shorts) where
  toLong lo = do
    (o, a) <- longVariableArg lo
    return (Right o, a)
  toShort lo = do
    (o, a) <- shortVariableArg lo
    return (Left o, a)
  f = toLong l
  longs = map toLong ls
  shorts = map toShort ss

