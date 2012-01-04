module System.Console.MultiArg.Combinator where

import Data.Text ( Text )
import Data.Set ( Set )
import Control.Monad ( liftM )

import System.Console.MultiArg.Prim
  ( (<|>), ParserSE, zero, try, approxLongOpt,
    nextArg, pendingShortOptArg, nonOptionPosArg,
    pendingShortOpt, nonPendingShortOpt, many,
    exactLongOpt )
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
