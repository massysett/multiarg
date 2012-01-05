module System.Console.MultiArg.Combinator where

import Data.Text ( Text, isPrefixOf )
import Data.Set ( Set )
import qualified Data.Set as Set
import Control.Monad ( liftM )

import System.Console.MultiArg.Prim
  ( (<|>), ParserSE, zero, try, approxLongOpt,
    nextArg, pendingShortOptArg, nonOptionPosArg,
    pendingShortOpt, nonPendingShortOpt, many,
    exactLongOpt, nextArg )
import System.Console.MultiArg.Option
  ( LongOpt, ShortOpt )
import qualified System.Console.MultiArg.Error as E
import System.Console.MultiArg.Error
  ( Error, unexpected )

option :: a -> ParserSE s e a -> ParserSE s e a
option x p = p <|> return x

optionMaybe :: ParserSE s e a -> ParserSE s e (Maybe a)
optionMaybe p = option Nothing (liftM Just p)

choice :: e -> [ParserSE s e a] -> ParserSE s e a
choice e ls = foldl (<|>) (zero e) ls

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

shortNoArg :: (Error e)
            => ShortOpt
            -> ParserSE s e ShortOpt
shortNoArg s = pendingShortOpt s <|> nonPendingShortOpt s

shortOptionalArg :: (Error e)
                 => ShortOpt
                 -> ParserSE s e (ShortOpt, Maybe Text)
shortOptionalArg s = do
  so <- shortNoArg s
  a <- optionMaybe (pendingShortOptArg <|> nonOptionPosArg)
  return (so, a)

shortSingleArg :: (Error e) =>
               ShortOpt
               -> ParserSE s e (ShortOpt, Text)
shortSingleArg s = do
  so <- shortNoArg s
  a <- pendingShortOptArg <|> nextArg
  return (so, a)

shortDoubleArg :: (Error e)
               => ShortOpt
               -> ParserSE s e (ShortOpt, Text, Text)
shortDoubleArg s = do
  (so, a1) <- shortSingleArg s
  a2 <- nextArg
  return (so, a1, a2)

shortVariableArg :: (Error e)
                 => ShortOpt
                 -> ParserSE s e (ShortOpt, [Text])
shortVariableArg s = do
  so <- shortNoArg s
  firstArg <- optionMaybe pendingShortOptArg
  rest <- many nonOptionPosArg
  let result = maybe rest ( : rest ) firstArg
  return (so, result)

longNoArg :: (Error e)
           => LongOpt
           -> ParserSE s e LongOpt
longNoArg = nonGNUexactLongOpt

longOptionalArg :: (Error e)
                   => LongOpt
                   -> ParserSE s e (LongOpt, Maybe Text)
longOptionalArg = exactLongOpt

longSingleArg :: (Error e)
                 => LongOpt
                 -> ParserSE s e (LongOpt, Text)
longSingleArg l = do
  (lo, mt) <- longOptionalArg l
  case mt of
    (Just t) -> return (lo, t)
    Nothing -> do
      a <- nextArg
      return (l, a)

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

longVariableArg :: (Error e)
                   => LongOpt
                   -> ParserSE s e (LongOpt, [Text])
longVariableArg l = do
  (lo, mt) <- longOptionalArg l
  rest <- many nonOptionPosArg
  return (lo, maybe rest (:rest) mt)

