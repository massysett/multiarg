-- | Combinators that are useful for building command-line
-- parsers. These build off the functions in
-- "System.Console.MultiArg.Prim". Unlike those functions, these
-- functions have no access to the internals of the parser.
module System.Console.MultiArg.Combinator (
  -- * Parser combinators
  notFollowedBy,

  -- * Combined long and short option parser
  OptSpec(OptSpec, longOpts, shortOpts, argSpec),
  OptArgError(..),
  reader,
  optReader,
  ArgSpec(..),
  parseOption,

  -- * Formatting errors
  formatError
  ) where

import Data.List (isPrefixOf, intersperse, nubBy)
import Data.Set ( Set )
import qualified Data.Set as Set
import Control.Applicative
       ((<$>), (<*>), optional, (<$), (*>), (<|>), many)

import qualified Control.Monad.Exception.Synchronous as Ex
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

-- | Reads in values that are members of Read. Provides a generic
-- error message if the read fails.
reader :: Read a => String -> Ex.Exceptional OptArgError a
reader s = case reads s of
  (a, ""):[] -> return a
  _ -> Ex.throw . ErrorMsg $ "could not parse option argument"

-- | Reads in values that are members of Read, but the value does not
-- have to appear on the command line. Provides a generic error
-- message if the read fails. If the argument is Nothing, returns
-- Nothing.
optReader
  :: Read a
  => Maybe String
  -> Ex.Exceptional OptArgError (Maybe a)
optReader ms = case ms of
  Nothing -> return Nothing
  Just s -> case reads s of
    (a, ""):[] -> return (Just a)
    _ -> Ex.throw . ErrorMsg $ "could not parse option argument"

-- | Indicates errors when parsing options to arguments.
data OptArgError
  = NoMsg
  -- ^ No error message accompanies this failure. multiarg will create
  -- a generic error message for you.

  | ErrorMsg String
  -- ^ Parsing the argument failed with this error message. An example
  -- might be @option argument is not an integer@ or @option argument
  -- is too large@. The text of the options the user provided is
  -- automatically prepended to the error message, so do not replicate
  -- this in your message.

  deriving (Eq, Show)

-- | Create an error message from an OptArgError.
errorMsg
  :: Either LongOpt ShortOpt
  -- ^ The option with the faulty argument

  -> [String]
  -- ^ The faulty command line arguments

  -> OptArgError
  -> String
errorMsg badOpt ss err = arg ++ opt ++ msg
  where
    arg = let aw = if length ss > 1 then "arguments " else "argument "
              ws = concat . intersperse " " . map quote $ ss
              quote s = "\"" ++ s ++ "\""
          in aw ++ ws
    opt = " to option " ++ optDesc
    optDesc = case badOpt of
      Left lo -> "--" ++ unLongOpt lo
      Right so -> "-" ++ [unShortOpt so]
    msg = "invalid" ++ detail
    detail = case err of
      NoMsg -> ""
      ErrorMsg s -> ": " ++ s



-- | Specifies how many arguments each option takes. As with
-- 'System.Console.GetOpt.ArgDescr', there are (at least) two ways to
-- use this type. You can simply represent each possible option using
-- different data constructors in an algebraic data type. Or you can
-- have each ArgSpec yield a function that transforms a record. For an
-- example that uses an algebraic data type, see
-- "System.Console.MultiArg.SampleParser".
--
-- Some of these value constructors have names ending in @E@. Use
-- these constructors when you want to parse option arguments that may
-- fail to parse--for example, you want to parse an Int. The function
-- passed as an argument to the value constructor indicates failure by
-- returing an 'Exception'.
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

  | OptionalArgE (Maybe String -> Ex.Exceptional OptArgError a)
    -- ^ This option takes an optional argument, like
    -- 'OptionalArg'. Parsing of the optional argument might fail.

  | OneArgE (String -> Ex.Exceptional OptArgError a)
    -- ^ This option takes a single argument, like 'OneArg'. Parsing
    -- of the argument might fail.

  | TwoArgE (String -> String -> Ex.Exceptional OptArgError a)
    -- ^ This option takes two arguments, like 'TwoArg'. Parsing of
    -- the arguments might fail.

  | ThreeArgE (String -> String -> String -> Ex.Exceptional OptArgError a)
    -- ^ This option takes three arguments, like 'ThreeArg'. Parsing
    -- of the arguments might fail.

  | VariableArgE ([String] -> Ex.Exceptional OptArgError a)
    -- ^ This option takes a variable number of arguments, like
    -- 'VariableArg'. Parsing of the arguments might fail.


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

    OptionalArgE g -> OptionalArgE $ \ms -> fmap f (g ms)

    OneArgE g ->
      OneArgE $ \s1 -> fmap f (g s1)

    TwoArgE g ->
      TwoArgE $ \s1 s2 -> fmap f (g s1 s2)

    ThreeArgE g ->
      ThreeArgE $ \s1 s2 s3 -> fmap f (g s1 s2 s3)

    VariableArgE g ->
      VariableArgE $ \ls -> fmap f (g ls)


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

    OptionalArgE f -> case maybeArg of
      Nothing -> Ex.switch (fail . errorMsg (Left lo) []) return
                 $ f Nothing
      Just s -> Ex.switch (fail . errorMsg (Left lo) [s]) return
                $ f (Just s)


    OneArgE f -> maybeNextArg >>= g
      where
        g a = Ex.switch (fail . errorMsg (Left lo) [a]) return
              $ f a

    TwoArgE f -> do
      a1 <- maybeNextArg
      a2 <- nextWord
      Ex.switch (fail . errorMsg (Left lo) [a1, a2]) return
        $ f a1 a2

    ThreeArgE f -> do
      a1 <- maybeNextArg
      a2 <- nextWord
      a3 <- nextWord
      Ex.switch (fail . errorMsg (Left lo) [a1, a2, a3]) return
        $ f a1 a2 a3

    VariableArgE f -> do
      as <- many nonOptionPosArg
      let args = case maybeArg of
            Nothing -> as
            Just a -> a:as
      Ex.switch (fail . errorMsg (Left lo) args) return
        $ f args


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
      OptionalArgE f -> shortOptionalArgE opt f
      OneArgE f -> shortOneArgE opt f
      TwoArgE f -> shortTwoArgE opt f
      ThreeArgE f -> shortThreeArgE opt f
      VariableArgE f -> shortVariableArgE opt f

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


shortVariableArgE
  :: ShortOpt
  -> ([String] -> Ex.Exceptional OptArgError a)
  -> Parser a
shortVariableArgE so f = do
  maybeSameWordArg <- optional pendingShortOptArg
  args <- many nonOptionPosArg
  let as = case maybeSameWordArg of
        Nothing -> args
        Just a -> a:args
  Ex.switch (fail . errorMsg (Right so) as) return $ f as


shortOneArg :: (String -> a) -> Parser a
shortOneArg f = f <$> firstShortArg

shortOneArgE
  :: ShortOpt
  -> (String -> Ex.Exceptional OptArgError a)
  -> Parser a
shortOneArgE so f = do
  a <- firstShortArg
  Ex.switch (fail . errorMsg (Right so) [a]) return $ f a

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

shortTwoArgE
  :: ShortOpt
  -> (String -> String -> Ex.Exceptional OptArgError a)
  -> Parser a
shortTwoArgE so f = do
  a1 <- firstShortArg
  a2 <- nextWord
  Ex.switch (fail . errorMsg (Right so) [a1, a2]) return
    $ f a1 a2

shortThreeArg :: (String -> String -> String -> a) -> Parser a
shortThreeArg f = f <$> firstShortArg <*> nextWord <*> nextWord

shortThreeArgE
  :: ShortOpt
  -> (String -> String -> String -> Ex.Exceptional OptArgError a)
  -> Parser a
shortThreeArgE so f = do
  a1 <- firstShortArg
  a2 <- nextWord
  a3 <- nextWord
  Ex.switch (fail . errorMsg (Right so) [a1, a2, a3]) return
    $ f a1 a2 a3

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

shortOptionalArgE
  :: ShortOpt
  -> (Maybe String -> Ex.Exceptional OptArgError a)
  -> Parser a
shortOptionalArgE so f = do
  maybeSameWordArg <- optional pendingShortOptArg
  case maybeSameWordArg of
    Nothing -> do
      maybeArg <- optional nonOptionPosArg
      case maybeArg of
        Nothing -> Ex.switch (fail . errorMsg (Right so) []) return
                   $ f Nothing
        Just a -> Ex.switch (fail . errorMsg (Right so) [a]) return
                  $ f (Just a)
    Just a -> Ex.switch (fail . errorMsg (Right so) [a]) return
              $ f (Just a)



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
    
