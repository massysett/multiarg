-- | Combinators that are useful for building command-line
-- parsers. These build off the functions in
-- "System.Console.MultiArg.Prim". Unlike those functions, these
-- functions have no access to the internals of the parser.
module System.Console.MultiArg.Combinator (
  -- * Parser combinators
  notFollowedBy,

  -- * Combined long and short option parser
  OptSpec(OptSpec, longOpts, shortOpts, argSpec),
  ArgSpec(NoArg, OptionalArg, OneArg, TwoArg,
          ThreeArg, VariableArg, ChoiceArg),
  parseOption,

  -- * Formatting errors
  formatError
  ) where

import Data.List (isPrefixOf, intersperse, nubBy)
import Data.Set ( Set )
import qualified Data.Set as Set
import Control.Applicative
       ((<$>), (<*>), optional, (<$), (*>), (<|>), many)

import System.Console.MultiArg.Prim
  ( Parser, try, approxLongOpt,
    nextWord, pendingShortOptArg, nonOptionPosArg,
    pendingShortOpt, nonPendingShortOpt, nextWord, (<?>),
    Error(..), Description(..))
import System.Console.MultiArg.Option
  ( LongOpt, ShortOpt, unLongOpt,
    makeLongOpt, makeShortOpt, unShortOpt )
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe (fromMaybe, mapMaybe)
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


unsafeShortOpt :: Char -> ShortOpt
unsafeShortOpt c =
  fromMaybe (error $ "invalid short option: " ++ [c])
            (makeShortOpt c)

unsafeLongOpt :: String -> LongOpt
unsafeLongOpt c =
  fromMaybe (error $ "invalid long option: " ++ c)
            (makeLongOpt c)


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

instance Functor OptSpec where
  fmap f (OptSpec ls ss as) = OptSpec ls ss (fmap f as)

-- | Specifies how many arguments each option takes. As with
-- 'System.Console.GetOpt.ArgDescr', there are (at least) two ways to
-- use this type. You can simply represent each possible option using
-- different data constructors in an algebraic data type. Or you can
-- have each ArgSpec yield a function that transforms a record. For an
-- example that uses an algebraic data type, see
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
    -- ^ This option takes two arguments. Parsed similarly to
    -- 'OneArg'.

  | ThreeArg (String -> String -> String -> a)
    -- ^ This option takes three arguments. Parsed similarly to
    -- 'OneArg'.

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

  | ChoiceArg [(String, a)]
    -- ^ This option takes a single argument, which must match one of
    -- the strings given in the list. The user may supply the shortest
    -- unambiguous string. If the argument list to ChoiceArg has
    -- duplicate strings, only the first string is used. For instance,
    -- ChoiceArg could be useful if you were parsing the @--color@
    -- option to GNU grep, which requires the user to supply one of
    -- three arguments: @always@, @never@, or @auto@.


instance Functor ArgSpec where
  fmap f a = case a of
    NoArg i -> NoArg $ f i
    OptionalArg g ->
      OptionalArg $ \ms -> f (g ms)
    OneArg g ->
      OneArg $ \s1 -> f (g s1)
    TwoArg g ->
      TwoArg $ \s1 s2 -> f (g s1 s2)
    ThreeArg g ->
      ThreeArg $ \s1 s2 s3 -> f (g s1 s2 s3)
    VariableArg g ->
      VariableArg $ \ls -> f (g ls)
    ChoiceArg gs ->
      ChoiceArg . map (\(s, r) -> (s, f r)) $ gs


-- | Parses a single command line option. Examines all the options
-- specified using multiple OptSpec and parses one option on the
-- command line accordingly. Fails without consuming any input if the
-- next word on the command line is not a recognized option. Allows
-- the user to specify the shortest unambiguous match for long
-- options; for example, the user could type @--verb@ for @--verbose@
-- and @--vers@ for @--version@.
--
-- This function is applied to a list of OptSpec, rather than to a
-- single OptSpec, because in order to correctly handle the parsing of
-- shortened long options (e.g. @--verb@ rather than @--verbose@) it
-- is necessary for one function to have access to all of the
-- OptSpec. Applying this function multiple times to different lists
-- of OptSpec and then using the @<|>@ function to combine them will
-- break the proper parsing of shortened long options.
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
      maybeNextArg = maybe nextWord return maybeArg
  case spec of
    NoArg a -> case maybeArg of
      Nothing -> return a
      Just _ -> fail $ "option " ++ unLongOpt lo
                  ++ " does not take argument"
    OptionalArg f -> return (f maybeArg)
    OneArg f -> f <$> maybeNextArg
    TwoArg f -> f <$> maybeNextArg <*> nextWord
    ThreeArg f -> f <$> maybeNextArg <*> nextWord <*> nextWord
    VariableArg f -> do
      as <- many nonOptionPosArg
      return . f $ case maybeArg of
        Nothing -> as
        Just a1 -> a1 : as
    ChoiceArg ls -> do
      s <- maybeNextArg
      case matchAbbrev ls s of
        Nothing -> fail $ "option " ++ unLongOpt lo
                   ++ " requires an argument: "
                   ++ (concat . intersperse ", " . map fst $ ls)
        Just g -> return g

shortOpt :: OptSpec a -> Maybe (Parser a)
shortOpt o = mconcat parsers where
  parsers = map mkParser . shortOpts $ o
  mkParser c =
    let opt = unsafeShortOpt c
    in Just $ nextShort opt *> case argSpec o of
      NoArg a -> return a
      OptionalArg f -> shortOptionalArg f
      OneArg f -> shortOneArg f
      TwoArg f -> shortTwoArg f
      ThreeArg f -> shortThreeArg f
      VariableArg f -> shortVariableArg f
      ChoiceArg ls -> shortChoiceArg opt ls

-- | Parses a short option without an argument, either pending or
-- non-pending. Fails with a single error message rather than two.
nextShort :: ShortOpt -> Parser ()
nextShort o = p <?> ("short option: -" ++ [unShortOpt o])
  where
    p = do
      r1 <- optional $ pendingShortOpt o
      case r1 of
        Just () -> return ()
        Nothing -> nonPendingShortOpt o


shortVariableArg :: ([String] -> a) -> Parser a
shortVariableArg f = do
  maybeSameWordArg <- optional pendingShortOptArg
  args <- many nonOptionPosArg
  case maybeSameWordArg of
    Nothing -> return (f args)
    Just arg1 -> return (f (arg1:args))


shortOneArg :: (String -> a) -> Parser a
shortOneArg f = f <$> firstShortArg


firstShortArg :: Parser String
firstShortArg =
  optional pendingShortOptArg >>= maybe nextWord return


shortChoiceArg :: ShortOpt -> [(String, a)] -> Parser a
shortChoiceArg opt ls =
  firstShortArg
  >>= maybe err return . matchAbbrev ls
  where
    err = fail $ "option " ++ [unShortOpt opt] ++ " requires "
          ++ "one argument: "
          ++ (concat . intersperse " " . map fst $ ls)



shortTwoArg :: (String -> String -> a) -> Parser a
shortTwoArg f = f <$> firstShortArg <*> nextWord

shortThreeArg :: (String -> String -> String -> a) -> Parser a
shortThreeArg f = f <$> firstShortArg <*> nextWord <*> nextWord

shortOptionalArg :: (Maybe String -> a) -> Parser a
shortOptionalArg f = do
  maybeSameWordArg <- optional pendingShortOptArg
  case maybeSameWordArg of
    Nothing -> do
      maybeArg <- optional nonOptionPosArg
      case maybeArg of
        Nothing -> return (f Nothing)
        Just a -> return (f (Just a))
    Just a -> return (f (Just a))

-- | Finds the unambiguous short match for a string, if there is
-- one. Returns a string describing the error condition if there is
-- one, or the matching result if successful.
matchAbbrev :: [(String, a)] -> String -> Maybe a
matchAbbrev ls s =
  let ls' = nubBy (\x y -> fst x == fst y) ls
  in case lookup s ls' of
    Just a -> return a
    Nothing ->
      let pdct (t, _) = s `isPrefixOf` t
      in case filter pdct ls of
        (_, a):[] -> return a
        _ -> Nothing

-- | Formats error messages for nice display. Returns a multi-line
-- string (there is no need to append a newline to the end of the
-- string returned).
formatError
  :: String
  -- ^ Pass the name of your program here. Displayed at the beginning
  -- of the error message.

  -> Error
  -> String
formatError p (Error loc ls) =
  p ++ ": error: could not parse command line.\n"
  ++ "Error at: " ++ loc ++ "\n"
  ++ expError
  ++ genError
  ++ unk
  where
    toExp m = case m of { Expected s -> Just s; _ -> Nothing }
    expc = unlines . mapMaybe toExp $ ls
    expError = if null expc then "" else "Expecting:\n" ++ expc
    toGeneral m = case m of { General s -> Just s; _ -> Nothing }
    gen = unlines . mapMaybe toGeneral $ ls
    genError = if null gen
               then ""
               else "Other errors:\n" ++ gen
    unk = if any (== Unknown) ls then "Unknown error\n" else ""
    
