-- | A simple command line parser that can parse options that take an
-- optional argument, one or two arguments, or a variable number of
-- arguments. For sample code that uses this parser, see
-- 'System.Console.MultiArg.SampleParser'.
module System.Console.MultiArg.SimpleParser (
  OptSpec(..),
  Intersperse(..),
  Result(..),
  Args(..),
  noArg,
  optionalArg,
  oneArg,
  twoArg,
  variableArg,
  SimpleError,
  getArgs,
  System.Console.MultiArg.SimpleParser.parse ) where

import System.Console.MultiArg.Prim (
  Parser, manyTill, lookAhead, nextArg, nonOptionPosArg,
  end, (<?>), stopper, nonOptionPosArg )
import qualified System.Console.MultiArg.Prim as Prim
import System.Console.MultiArg.Combinator (
  mixedNoArg, mixedOptionalArg, mixedOneArg, mixedTwoArg,
  mixedVariableArg )
import System.Console.MultiArg.Option (
  makeLongOpt, makeShortOpt )
import Control.Monad.Exception.Synchronous ( toEither )
import System.Console.MultiArg.Error ( SimpleError )
import Data.Text ( pack, unpack )
import System.Environment ( getArgs )
import Data.Monoid ( mconcat )
import Control.Applicative ( many, (<|>) )

-- | Specifies each option that your program accepts.
data OptSpec = OptSpec {
  longOpt :: String
  -- ^ Each option must have at least one long option, which you
  -- specify here. Your program's users specify long options by
  -- preceding them with two dashes, such as @--verbose@. When writing
  -- your code you omit the dashes, so you would specify @verbose@
  -- here; including the dashes in your code results in a runtime
  -- error.

  , shortOpts :: [Char]
    -- ^ Additional, synonymous short options may be specified
    -- here. For instance if you want your users to be able to specify
    -- @-v@ in addition to @--verbose@, include @v@ in this list.
    
  , longOpts :: [String]
    -- ^ Additional synonymous long options may be specified here. For
    -- instance, if you specified @quiet@ for @longOpt@, you might
    -- want to include @silent@ in this list.

  , argSpec :: Args
    -- ^ Specifies what arguments, if any, this option takes.
  }
             deriving Show

-- | This datatype does dual duty. When part of an 'OptSpec', you use
-- it to specify how many arguments, if any, an option takes. When you
-- use it for this purpose, it only matters which data constructor you
-- use; the fields can be any value, even 'undefined'.
--
-- When part of a Result, 'Args' indicates what arguments the user
-- supplied to the option.
data Args =
  NoArg
  -- ^ This option takes no arguments
          
  | OptionalArg  { oArg :: Maybe String }

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

  | OneArg       { sArg1 :: String }
    -- ^ This option takes one argument. Here, if option @a@ takes one
    -- argument, @-a -b@ will be parsed with @-b@ being an argument to
    -- option @a@, even though @-b@ starts with a hyphen and therefore
    -- \"looks like\" an option.
    
  | TwoArg       { tArg1 :: String
                 , tArg2 :: String }
    -- ^ This option takes two arguments. Parsed similarly to 'OneArg'.
    
  | VariableArg { vArgs :: [String] }
    -- ^ This option takes a variable number of arguments--zero or
    -- more. Option arguments continue until the command line contains
    -- a word that begins with a hyphen. For example, if option @a@
    -- takes a variable number of arguments, then @-a one two three
    -- -b@ will be parsed as @a@ taking three arguments, and @-a -b@
    -- will be parsed as @a@ taking no arguments. If the user enters
    -- @-a@ as the last option on the command line, then the only way
    -- to indicate the end of arguments for @a@ and the beginning of
    -- positional argments is with a stopper.
    
  deriving Show

-- | Specify that this option takes no arguments.
noArg :: Args
noArg = NoArg

-- | Specify that this option takes an optional argument.
optionalArg :: Args
optionalArg = OptionalArg Nothing

-- | Specify that this option takes one argument.
oneArg :: Args
oneArg = OneArg ""

-- | Specify that this option takes two arguments.
twoArg :: Args
twoArg = TwoArg "" ""

-- | Specify that this option takes a variable number of arguments.
variableArg :: Args
variableArg = VariableArg []

-- | Holds the result of command line parsing. Each option (along with
-- its option arguments) or positional argument is assigned to its own
-- Result.
data Result =
  PosArg   { posArg :: String }
  | Stopper
  | Option {
    label :: String
    -- ^ Each option must have at least one long option. So that you
    -- can distinguish one option from another, the name of that long
    -- option is returned here.
    , args :: Args }
              deriving Show

-- | What to do after encountering the first non-option,
-- non-option-argument word on the command line? In either case, no
-- more options are parsed after a stopper.
data Intersperse =
  Intersperse
  -- ^ Additional options are allowed on the command line after
  -- encountering the first positional argument. For example, if @a@
  -- and @b@ are options, in the command line @-a posarg -b@, @b@ will
  -- be parsed as an option. If @b@ is /not/ an option and the same
  -- command line is entered, then @-b@ will result in an error
  -- because @-b@ starts with a hyphen and therefore \"looks like\" an
  -- option.
  
  | StopOptions
    -- ^ No additional options will be parsed after encountering the
    -- first positional argument. For example, if @a@ and @b@ are
    -- options, in the command line @-a posarg -b@, @b@ will be parsed
    -- as a positional argument rather than as an option.

-- | Parse a command line. 
parse ::
  Intersperse
  -> [OptSpec]
  -> [String]
  -- ^ The command line to parse. This function correctly handles
  -- Unicode strings; however, because 'System.Environment.getArgs'
  -- does not always correctly handle Unicode strings, consult the
  -- documentation in 'System.Console.MultiArg.GetArgs' and consider
  -- using the functions in there if there is any chance that you will
  -- be parsing command lines that have non-ASCII strings.

  -> Either SimpleError [Result]
parse i os ss = toEither $ Prim.parse (map pack ss) (f os) where
  f = case i of Intersperse -> parseIntersperse
                StopOptions -> parseNoIntersperse

parseNoIntersperse :: [OptSpec] -> Parser [Result]
parseNoIntersperse os = do
  let opts = mconcat . map optSpec $ os
  rs <- manyTill opts (lookAhead afterArgs)
  firstArg <- afterArgs
  case firstArg of
    EndOfInput -> return rs
    (FirstArg s) -> do
      as <- noIntersperseArgs
      let first = PosArg s
      return $ rs ++ ( first : as )
    AAStopper -> do
      as <- noIntersperseArgs
      let first = Stopper
      return $ rs ++ ( first : as )

noIntersperseArgs :: Parser [Result]
noIntersperseArgs = do
  as <- many nextArg
  let r = map PosArg . map unpack $ as
  return r

data AfterArgs = EndOfInput | FirstArg String | AAStopper

afterArgs :: Parser AfterArgs
afterArgs = parseFirst <|> parseEnd <|> parseStopper where
  parseFirst = do
    a <- nonOptionPosArg
    let aS = unpack a
    return $ FirstArg aS
  parseEnd = do
    end
    return EndOfInput
  parseStopper = do
    _ <- stopperParser
    return AAStopper

parseIntersperse :: [OptSpec] -> Parser [Result]
parseIntersperse os = do
  let optsAndStopper = foldl1 (<|>) $ optSpecs ++ rest
      rest = [stopperParser, posArgParser]
      optSpecs = map optSpec os
  rs <- manyTill optsAndStopper end
  end <?> error "the end parser should always succeed"
  return rs

stopperParser :: Parser Result
stopperParser = stopper >> return Stopper

posArgParser :: Parser Result
posArgParser = do
  a <- nonOptionPosArg
  return $ PosArg (unpack a)

optSpec :: OptSpec -> Parser Result
optSpec o = let
  lo = makeLongOpt . pack . longOpt $ o
  ss = map makeShortOpt . shortOpts $ o
  ls = map makeLongOpt . map pack . longOpts $ o
  opt = return . Option (longOpt o)
  in case argSpec o of
    NoArg -> do
      _ <- mixedNoArg lo ls ss
      opt NoArg
    (OptionalArg {}) -> do
      (_, a) <- mixedOptionalArg lo ls ss
      opt (OptionalArg . fmap unpack $ a)
    (OneArg {}) -> do
      (_, a) <- mixedOneArg lo ls ss
      opt (OneArg . unpack $ a)
    (TwoArg {}) -> do
      (_, a1, a2) <- mixedTwoArg lo ls ss
      opt (TwoArg (unpack a1) (unpack a2))
    (VariableArg {}) -> do
      (_, as) <- mixedVariableArg lo ls ss
      opt (VariableArg . map unpack $ as)

