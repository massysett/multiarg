-- | Combinators that are useful for building command-line
-- parsers. These build off the functions in
-- "System.Console.MultiArg.Prim". Unlike those functions, these
-- functions have no access to the internals of the parser.
module System.Console.MultiArg.Combinator (
  -- * Parser combinators
  notFollowedBy,
  
  -- * Miscellaneous tests
  nextLooksLong,
  nextLooksShort,

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
    lookAhead,
    Message(Expected), (<?>))
import System.Console.MultiArg.Option
  ( LongOpt, ShortOpt, unLongOpt,
    makeLongOpt, makeShortOpt, unShortOpt )
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
      err = throw $ Expected
            ("word matching one of: "
             ++ (concat . intersperse ", " $ Set.toList s))
  case Set.toList matches of
    [] -> err
    (x:[]) -> return (a, x)
    _ -> err


-- | Returns True if the next argument on the command line looks like
-- a long option--that is, it is preceded by two dashes. Does not
-- consume any input. Never fails.
nextLooksLong :: Parser Bool
nextLooksLong = do
  maybeNext <- optional (lookAhead nextArg)
  return $ case maybeNext of
    Nothing -> False
    Just n ->
      if "--" `isPrefixOf` n && (length n /= 2)
      then True else False

-- | Returns True if the next argument on the command line looks like
-- a short option--that is, it is preceded by one dash. Does not
-- consume any input. Never fails.
nextLooksShort :: Parser Bool
nextLooksShort = do
  maybeNext <- optional (lookAhead nextArg)
  return $ case maybeNext of
    Nothing -> False
    Just n ->
      if "-" `isPrefixOf` n && (n /= "--")
      then True else False

unsafeShortOpt :: Char -> ShortOpt
unsafeShortOpt c = case makeShortOpt c of
  Nothing -> error $ "invalid short option: " ++ [c]
  Just o -> o

unsafeLongOpt :: String -> LongOpt
unsafeLongOpt c = case makeLongOpt c of
  Nothing -> error $ "invalid long option: " ++ c
  Just o -> o


-- |Specifies options for the 'parseOption' function. Each OptSpec
-- represents one command-line option.
data OptSpec a = OptSpec {
  longOpts :: [String]
  -- ^ Each String is a single long option, such as @version@. When
  -- the user specifies long options on the command line, she must
  -- type two dashes; however, do not include the dashes when you
  -- specify the long option here. Strings you specify as long options
  -- cannot include a dash as either the first or the second
  -- character, and they cannot include an equal sign anywhere. If
  -- your long option does not meet these conditions, a runtime error
  -- will occur.

          
  , shortOpts :: [Char]
    -- ^ Each Char is a single short option, such as @v@. The
    -- character cannot be a dash; if it is, a runtime error will occur.
    
  , argSpec :: ArgSpec a
    -- ^ What to do each time one of the given long options or
    -- short options appears on the command line.
  }

-- | Specifies how many arguments each option takes. As with
-- 'System.Console.GetOpt.ArgDescr', there are (at least) two ways to
-- use this type. You can simply represent each possible option using
-- different data constructors in an algebraic data type. Or you can
-- have each ArgSpec yield a function that transforms a record. For an
-- example that uses an algebraid data type, see
-- "System.Console.MultiArg.SampleParser".
data ArgSpec a =
  NoArg a
  -- ^ This option takes no arguments

  | OptionalArg (Maybe String -> a)
    -- ^ This option takes an optional argument. As noted in \"The Tao
    -- of Option Parsing\", optional arguments can result in some
    -- ambiguity. (Read it here:
    -- <http://optik.sourceforge.net/doc/1.5/tao.html>) If option @a@
    -- takes an optional argument, and @b@ is also an option, what
    -- does @-ab@ mean? SimpleParser resolves this ambiguity by
    -- assuming that @b@ is an argument to @a@. If the user does not
    -- like this, she can specify @-a -b@ (in such an instance @-b@ is
    -- not parsed as an option to @-a@, because @-b@ begins with a
    -- hyphen and therefore \"looks like\" an option.) Certainly
    -- though, optional arguments lead to ambiguity, so if you don't
    -- like it, don't use them :)

  | OneArg (String -> a)
    -- ^ This option takes one argument. Here, if option @a@ takes one
    -- argument, @-a -b@ will be parsed with @-b@ being an argument to
    -- option @a@, even though @-b@ starts with a hyphen and therefore
    -- \"looks like\" an option.
    
  | TwoArg (String -> String -> a)
    -- ^ This option takes two arguments. Parsed similarly to 'OneArg'.

  | VariableArg ([String] -> a)
    -- ^ This option takes a variable number of arguments--zero or
    -- more. Option arguments continue until the command line contains
    -- a word that begins with a hyphen. For example, if option @a@
    -- takes a variable number of arguments, then @-a one two three
    -- -b@ will be parsed as @a@ taking three arguments, and @-a -b@
    -- will be parsed as @a@ taking no arguments. If the user enters
    -- @-a@ as the last option on the command line, then the only way
    -- to indicate the end of arguments for @a@ and the beginning of
    -- positional argments is with a stopper.
    

-- | Parses a single command line option. Examines all the options
-- specified using multiple OptSpec and parses one option on the
-- command line accordingly. Fails without consuming any input if the
-- next word on the command line is not a recognized option. Allows
-- the user to specify the shortest unambiguous match for long
-- options; for example, the user could type @--verb@ for @--verbose@
-- and @--vers@ for @--version@.
--
-- For an example that uses this function, see
-- "System.Console.MultiArg.SimpleParser".
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
        a1 <- nextArg
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
      NoArg a -> a <$ (pendingShortOpt opt <|> nonPendingShortOpt opt
                       <?> ("short option: " ++ [c]))
      OptionalArg f -> shortOptionalArg opt f
      OneArg f -> shortOneArg opt f
      TwoArg f -> shortTwoArg opt f
      VariableArg f -> shortVariableArg opt f

shortVariableArg :: ShortOpt -> ([String] -> a) -> Parser a
shortVariableArg opt f = do
  pendingShortOpt opt <|> nonPendingShortOpt opt
    <?> ("short option: " ++ [unShortOpt opt])
  maybeSameWordArg <- optional pendingShortOptArg
  args <- many nonOptionPosArg
  case maybeSameWordArg of
    Nothing -> return (f args)
    Just arg1 -> return (f (arg1:args))
  

shortTwoArg :: ShortOpt -> (String -> String -> a) -> Parser a
shortTwoArg opt f = do
  pendingShortOpt opt <|> nonPendingShortOpt opt
    <?> ("short option: " ++ [unShortOpt opt])
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
  pendingShortOpt opt <|> nonPendingShortOpt opt
    <?> ("short option: " ++ [unShortOpt opt])
  maybeSameWordArg <- optional pendingShortOptArg
  case maybeSameWordArg of
    Nothing -> do
      arg <- nextArg
      return (f arg)
    Just a -> return (f a)


shortOptionalArg :: ShortOpt -> (Maybe String -> a) -> Parser a
shortOptionalArg opt f = do
  pendingShortOpt opt <|> nonPendingShortOpt opt
    <?> ("short option: " ++ [unShortOpt opt])
  maybeSameWordArg <- optional pendingShortOptArg
  case maybeSameWordArg of
    Nothing -> do
      maybeArg <- optional nonOptionPosArg
      case maybeArg of
        Nothing -> return (f Nothing)
        Just a -> return (f (Just a))
    Just a -> return (f (Just a))
