-- | Combinators that are useful for building command-line
-- parsers. These build off the functions in
-- "System.Console.MultiArg.Prim". Unlike those functions, these
-- functions have no access to the internals of the parser.
module System.Console.MultiArg.Combinator (
  -- * Parser combinators
  notFollowedBy,
  
  -- * Short options
  shortNoArg,
  shortOptionalArg,
  shortOneArg,
  shortTwoArg,
  shortVariableArg,

  -- * Long options
  nonGNUexactLongOpt,
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
  
  -- * Combined long and short option parser
  OptSpec(OptSpec, longOpts, shortOpts, argSpec),
  ArgSpec(NoArg, OptionalArg, OneArg, TwoArg, VariableArg),
  parseOption,
  
  -- * Other words
  matchApproxWord ) where
  
import Data.List (isPrefixOf, intersperse)
import Data.Set ( Set )
import qualified Data.Set as Set
import Control.Applicative ((<*>), (<$>), optional, (<$),
                            (*>))

import System.Console.MultiArg.Prim
  ( Parser, throw, try, approxLongOpt,
    nextArg, pendingShortOptArg, nonOptionPosArg,
    pendingShortOpt, nonPendingShortOpt,
    exactLongOpt, nextArg, (<?>),
    Error(Expected))
import System.Console.MultiArg.Option
  ( LongOpt, ShortOpt, unLongOpt,
    makeLongOpt, makeShortOpt )
import Control.Applicative ((<|>), many)
import Control.Monad ( void, replicateM )
import qualified Data.Map as M
import Data.Map ((!))
import Data.Monoid ( mconcat )
import Data.Maybe (catMaybes)

-- | @notFollowedBy p@ succeeds only if parser p fails. If p fails,
-- notFollowedBy succeeds without consuming any input. If p succeeds
-- and consumes input, notFollowedBy fails and consumes input. If p
-- succeeds and does not consume any input, notFollowedBy fails and
-- does not consume any input.
notFollowedBy :: Parser a -> Parser ()
notFollowedBy p =
  void $ ((try p >> fail "notFollowedBy failed")
          <|> return ())


-- | Parses only a non-GNU style long option (that is, one that does
-- not take option arguments by attaching them with an equal sign,
-- such as @--lines=20@).
nonGNUexactLongOpt :: LongOpt -> Parser LongOpt
nonGNUexactLongOpt l = try $ do
  maybeArg <- exactLongOpt l
  case maybeArg of
    Nothing -> return lo
    (Just _) ->
      let e = "option " ++ unLongOpt l ++ " does not take an argument"
      in fail e


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

-- | Parses short options that do not take any argument. (It is
-- however okay for the short option to be combined with other short
-- options in the same word.)
shortNoArg :: ShortOpt -> Parser ()
shortNoArg s = pendingShortOpt s <|> nonPendingShortOpt s

-- | Parses short options that take an optional argument. The argument
-- can be combined in the same word with the short option (@-c42@) or
-- can be in the ext word (@-c 42@).
shortOptionalArg :: ShortOpt -> Parser (Maybe String)
shortOptionalArg s = do
  shortNoArg s
  a <- optional (pendingShortOptArg <|> nonOptionPosArg)
  return a

-- | Parses short options that take a required argument.  The argument
-- can be combined in the same word with the short option (@-c42@) or
-- can be in the ext word (@-c 42@).
shortOneArg :: ShortOpt -> Parser String
shortOneArg s =
  shortNoArg s *> (pendingShortOptArg <|> nextArg)


-- | Parses short options that take two required arguments. The first
-- argument can be combined in the same word with the short option
-- (@-c42@) or can be in the ext word (@-c 42@). The next argument
-- will have to be in a separate word.
shortTwoArg :: ShortOpt -> Parser (String, String)
shortTwoArg s =
  (,) <$> shortOneArg s <*> nextArg


-- | Parses short options that take a variable number of
-- arguments. This will keep on parsing option arguments until it
-- encounters one that does not "look like" an option--that is, until
-- it encounters one that begins with a dash. Therefore, the only way
-- to terminate a variable argument option if it is the last option is
-- with a stopper. The first argument can be combined in the same word
-- with the short option (@-c42@) or can be in the ext word (@-c
-- 42@). Subsequent arguments will have to be in separate words.
shortVariableArg :: ShortOpt -> Parser [String]
shortVariableArg s = do
  shortNoArg s
  firstArg <- optional pendingShortOptArg
  rest <- many nonOptionPosArg
  let result = maybe rest ( : rest ) firstArg
  return result

-- | Parses long options that do not take any argument.
longNoArg :: LongOpt -> Parser ()
longNoArg = nonGNUexactLongOpt

-- | Parses long options that take a single, optional argument. The
-- single argument can be given GNU-style (@--lines=20@) or non-GNU
-- style in separate words (@lines 20@).
longOptionalArg :: LongOpt -> Parser (Maybe String)
longOptionalArg = exactLongOpt

-- | Parses long options that take a single, required argument. The
-- single argument can be given GNU-style (@--lines=20@) or non-GNU
-- style in separate words (@lines 20@).
longOneArg :: LongOpt -> Parser String
longOneArg l = do
  mt <- longOptionalArg l
  case mt of
    (Just t) -> return t
    Nothing -> do
      a <- nextArg
           <?> ("option " ++ unLongOpt l ++ "requires an argument")
      return a

-- | Parses long options that take a double, required argument. The
-- first argument can be given GNU-style (@--lines=20@) or non-GNU
-- style in separate words (@lines 20@). The second argument will have
-- to be in a separate word.
longTwoArg :: LongOpt -> Parser (String, String)
longTwoArg l = do
  mt <- longOptionalArg l
  case mt of
    (Just t) -> do
      a2 <- nextArg
      return (t, a2)
    Nothing -> do
      a1 <- nextArg
      a2 <- nextArg
      return (a1, a2)

-- | Parses long options that take a variable number of
-- arguments. This will keep on parsing option arguments until it
-- encounters one that does not "look like" an option--that is, until
-- it encounters one that begins with a dash. Therefore, the only way
-- to terminate a variable argument option if it is the last option is
-- with a stopper. The first argument can be combined in the same word
-- with the short option (@--lines=20@) or can be in the ext word
-- (@--lines 42@). Subsequent arguments will have to be in separate
-- words.
longVariableArg :: LongOpt -> Parser [String]
longVariableArg l = do
  mt <- longOptionalArg l
  rest <- many nonOptionPosArg
  return (maybe rest (:rest) mt)

-- | Parses at least one long option and a variable number of short
-- and long options that take no arguments.
mixedNoArg ::
  LongOpt
  -> [LongOpt]
  -> [ShortOpt]
  -> Parser (Either ShortOpt LongOpt)
mixedNoArg l ls ss = mconcat ([f] ++ longs ++ shorts) where
  toLong lo = do
    longNoArg lo
    return $ Right lo
  toShort so = do
    shortNoArg so
    return $ Left so
  f = toLong l
  longs = map toLong ls
  shorts = map toShort ss

-- | Parses at least one long option and a variable number of short
-- and long options that take an optional argument.
mixedOptionalArg ::
  LongOpt
  -> [LongOpt]
  -> [ShortOpt]
  -> Parser ((Either ShortOpt LongOpt), Maybe String)
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
  LongOpt
  -> [LongOpt]
  -> [ShortOpt]
  -> Parser ((Either ShortOpt LongOpt), String)
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
  LongOpt
  -> [LongOpt]
  -> [ShortOpt]
  -> Parser ((Either ShortOpt LongOpt), String, String)
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
  LongOpt
  -> [LongOpt]
  -> [ShortOpt]
  -> Parser ((Either ShortOpt LongOpt), [String])
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
  let ls = [noArgs, optArgs, oneArgs, twoArgs, varArgs] <*> [os]
  in case mconcat ls of
    Nothing -> error "no options given to parse."
    Just p -> p
  
noArgs :: [OptSpec a] -> Maybe (Parser a)
noArgs os = undefined

noArgShort :: a -> [Char] -> Maybe (Parser a)
noArgShort a = mconcat . map toParser where
  toParser c = Just (a <$ shortNoArg (unsafeShortOpt c))

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
longOpt set map = do
  (_, lo, maybeArg) <- approxLongOpt set
  let spec = map ! lo
  case spec of
    NoArg a -> case maybeArg of
      Nothing -> return a
      Just arg -> fail $ "option " ++ unLongOpt lo
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
shortOpt = undefined
{-
shortOpt o = mconcat parsers where
  parsers = map mkParser . shortOpts $ o
  mkParser c =
    let opt = unsafeShortOpt c
    in Just $ case argSpec o of
      NoArg a -> a <$ shortNoArg opt
      OptionalArg f -> do
        (_, maybeStr) <- 
-}
{-
noArgs os = shorts <|> longs where
  opts = filter p os where
    p (OptSpec _ _ as) = case as of
      NoArg _ -> True
      _ -> False
-}

optArgs :: [OptSpec a] -> Maybe (Parser a)
optArgs = undefined

oneArgs :: [OptSpec a] -> Maybe (Parser a)
oneArgs = undefined

twoArgs :: [OptSpec a] -> Maybe (Parser a)
twoArgs = undefined

varArgs :: [OptSpec a] -> Maybe (Parser a)
varArgs = undefined

