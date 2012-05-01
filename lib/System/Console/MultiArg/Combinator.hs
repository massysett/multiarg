-- | Combinators that are useful for building command-line
-- parsers. These build off the functions in
-- "System.Console.MultiArg.Prim". Unlike those functions, these
-- functions have no access to the internals of the parser.
module System.Console.MultiArg.Combinator (
  -- * Parser combinators
  notFollowedBy,
  
  -- * Combined long and short option parser
  OptSpec(OptSpec, longOpts, shortOpts, argSpec),
  ArgSpec(NoArg, OptionalArg, OneArg, TwoArg, VariableArg),
  parseOption,
  
  -- * Other words
  matchApproxWord ) where
  
import Data.List (isPrefixOf, intersperse)
import Data.Set ( Set )
import qualified Data.Set as Set
import Control.Applicative ((<*>), optional, (<$))

import System.Console.MultiArg.Prim
  ( Parser, throw, try, approxLongOpt,
    nextArg, pendingShortOptArg, nonOptionPosArg,
    pendingShortOpt, nonPendingShortOpt, nextArg,
    Error(Expected))
import System.Console.MultiArg.Option
  ( LongOpt, ShortOpt, unLongOpt,
    makeLongOpt, makeShortOpt )
import Control.Applicative ((<|>), many)
import qualified Data.Map as M
import Data.Map ((!))
import Data.Monoid ( mconcat )


-- | @notFollowedBy p@ succeeds only if parser p fails. If p fails,
-- notFollowedBy succeeds without consuming any input. If p succeeds
-- and consumes input, notFollowedBy fails and consumes input. If p
-- succeeds and does not consume any input, notFollowedBy fails and
-- does not consume any input.
notFollowedBy :: Parser a -> Parser ()
notFollowedBy p =
  () <$ ((try p >> fail "notFollowedBy failed")
         <|> return ())


-- | Examines the possible words in Set. If there are no pendings,
-- then get the next word and see if it matches one of the words in
-- Set. If so, returns the word actually parsed and the matching word
-- from Set. If there is no match, fails without consuming any input.
matchApproxWord :: Set String -> Parser (String, String)
matchApproxWord s = try $ do
  a <- nextArg
  let p t = a `isPrefixOf` t
      matches = Set.filter p s
      err saw = throw $ Expected
                ("word matching one of: "
                 ++ (concat . intersperse ", " $ Set.toList s))
                saw
  case Set.toList matches of
    [] -> err "no matches"
    (x:[]) -> return (a, x)
    _ ->
      let msg = "word is ambiguous: " ++ a
      in err msg

unsafeShortOpt :: Char -> ShortOpt
unsafeShortOpt c = case makeShortOpt c of
  Nothing -> error $ "invalid short option: " ++ [c]
  Just o -> o

unsafeLongOpt :: String -> LongOpt
unsafeLongOpt c = case makeLongOpt c of
  Nothing -> error $ "invalid long option: " ++ c
  Just o -> o


data OptSpec a =
  OptSpec { longOpts :: [String]
          , shortOpts :: [Char]
          , argSpec :: ArgSpec a }

data ArgSpec a =
  NoArg a
  | OptionalArg (Maybe String -> a)
  | OneArg (String -> a)
  | TwoArg (String -> String -> a)
  | VariableArg ([String] -> a)

parseOption :: [OptSpec a] -> Parser a
parseOption os =
  let longs = longOptParser os
  in case mconcat ([shortOpt] <*> os) of
    Nothing -> longs
    Just shorts -> longs <|> shorts
  
longOptParser :: [OptSpec a] -> Parser a
longOptParser os = longOpt (longOptSet os) (longOptMap os)
      

longOptSet :: [OptSpec a] -> Set LongOpt
longOptSet = Set.fromList . concatMap toOpts where
  toOpts = map unsafeLongOpt . longOpts

longOptMap :: [OptSpec a] -> M.Map LongOpt (ArgSpec a)
longOptMap = M.fromList . concatMap toPairs where
  toPairs (OptSpec los _ as) = map (toPair as) los where
    toPair a s = (unsafeLongOpt s, a)

longOpt ::
  Set LongOpt
  -> M.Map LongOpt (ArgSpec a)
  -> Parser a
longOpt set mp = do
  (_, lo, maybeArg) <- approxLongOpt set
  let spec = mp ! lo
  case spec of
    NoArg a -> case maybeArg of
      Nothing -> return a
      Just _ -> fail $ "option " ++ unLongOpt lo
                  ++ " does not take argument"
    OptionalArg f -> return (f maybeArg)
    OneArg f -> case maybeArg of
      Nothing -> do
        a1 <- nonOptionPosArg
        return $ f a1
      Just a -> return $ f a
    TwoArg f -> case maybeArg of
      Nothing -> do
        a1 <- nextArg
        a2 <- nextArg
        return $ f a1 a2
      Just a1 -> do
        a2 <- nextArg
        return $ f a1 a2
    VariableArg f -> do
      as <- many nonOptionPosArg
      return . f $ case maybeArg of
        Nothing -> as
        Just a1 -> a1 : as


shortOpt :: OptSpec a -> Maybe (Parser a)
shortOpt o = mconcat parsers where
  parsers = map mkParser . shortOpts $ o
  mkParser c =
    let opt = unsafeShortOpt c
    in Just $ case argSpec o of
      NoArg a -> a <$ (pendingShortOpt opt <|> nonPendingShortOpt opt)
      OptionalArg f -> shortOptionalArg opt f
      OneArg f -> shortOneArg opt f
      TwoArg f -> shortTwoArg opt f
      VariableArg f -> shortVariableArg opt f

shortVariableArg :: ShortOpt -> ([String] -> a) -> Parser a
shortVariableArg opt f = do
  (pendingShortOpt opt <|> nonPendingShortOpt opt)
  maybeSameWordArg <- optional pendingShortOptArg
  args <- many nonOptionPosArg
  case maybeSameWordArg of
    Nothing -> return (f args)
    Just arg1 -> return (f (arg1:args))
  

shortTwoArg :: ShortOpt -> (String -> String -> a) -> Parser a
shortTwoArg opt f = do
  (pendingShortOpt opt <|> nonPendingShortOpt opt)
  maybeSameWordArg <- optional pendingShortOptArg
  case maybeSameWordArg of
    Nothing -> do
      arg1 <- nextArg
      arg2 <- nextArg
      return (f arg1 arg2)
    Just arg1 -> do 
      arg2 <- nextArg
      return (f arg1 arg2)

  
  

shortOneArg :: ShortOpt -> (String -> a) -> Parser a
shortOneArg opt f = do
  (pendingShortOpt opt <|> nonPendingShortOpt opt)
  maybeSameWordArg <- optional pendingShortOptArg
  case maybeSameWordArg of
    Nothing -> do
      arg <- nextArg
      return (f arg)
    Just a -> return (f a)
  

shortOptionalArg :: ShortOpt -> (Maybe String -> a) -> Parser a
shortOptionalArg opt f = do
  (pendingShortOpt opt <|> nonPendingShortOpt opt)
  maybeSameWordArg <- optional pendingShortOptArg
  case maybeSameWordArg of
    Nothing -> do
      maybeArg <- optional nonOptionPosArg
      case maybeArg of
        Nothing -> return (f Nothing)
        Just a -> return (f (Just a))
    Just a -> return (f (Just a))
