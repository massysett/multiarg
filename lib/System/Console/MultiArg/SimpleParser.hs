-- | A simple command line parser that can parse options that take an
-- optional argument, one or two arguments, or a variable number of
-- arguments. For sample code that uses this parser, see
-- "System.Console.MultiArg.SampleParser".
module System.Console.MultiArg.SimpleParser (
  -- * Interspersion control
  Intersperse (Intersperse, StopOptions)

  -- * Option specifications
  , C.OptSpec (OptSpec, longOpts, shortOpts, argSpec)
  , C.ArgSpec (NoArg, OptionalArg, OneArg, TwoArg, VariableArg)

    -- * Exceptions
  , Ex.Exceptional (Exception, Success)
  , P.Error (Error)
  , P.Message (Expected, StrMsg, Replaced, UnknownError)

    -- * Get command line arguments
  , G.getArgs

    -- * The parser
  , parse

    -- * Parsing multi-command command lines
  , Command(..)
  , commands
  ) where

import qualified System.Console.MultiArg.Prim as P
import qualified System.Console.MultiArg.GetArgs as G
import qualified System.Console.MultiArg.Combinator as C
import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Applicative ( many, (<|>), optional,
                             (<$), (<*>), (<*), (<$>))
import Data.List (find)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set


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
  -- ^ What to do after encountering the first positional argument

  -> [C.OptSpec a]
  -- ^ All possible options

  -> (String -> a)
  -- ^ How to handle positional arguments. This function is applied to
  -- the appropriate string every time the parser encounters a
  -- positional argument.

  -> [String]
  -- ^ The command line to parse. This function correctly handles
  -- Unicode strings; however, because 'System.Environment.getArgs'
  -- does not always correctly handle Unicode strings, consult the
  -- documentation in 'System.Console.MultiArg.GetArgs' and consider
  -- using the functions in there if there is any chance that you will
  -- be parsing command lines that have non-ASCII strings.

  -> Ex.Exceptional P.Error [a]
parse i os p as =
  let optParser = C.parseOption os
      parser = case i of
        Intersperse -> parseIntersperse optParser p
        StopOptions -> parseStopOpts optParser p
  in P.parse as parser

parseOptsNoIntersperse :: P.Parser a -> P.Parser [a]
parseOptsNoIntersperse p = P.manyTill p e where
  e = P.end <|> nonOpt
  nonOpt = P.lookAhead next
  next = (() <$ P.nonOptionPosArg) <|> P.stopper


parseStopOpts :: P.Parser a -> (String -> a) -> P.Parser [a]
parseStopOpts optParser p =
  (\opts args -> opts ++ map p args)
  <$> parseOptsNoIntersperse optParser
  <* optional P.stopper
  <*> many P.nextArg


-- | @parseIntersperse o p@ parses options and positional arguments,
-- where o is a parser that parses options, and p is a function that,
-- when applied to a string, returns the appropriate type.
parseIntersperse :: P.Parser a -> (String -> a) -> P.Parser [a]
parseIntersperse optParser p =
  let pa = (Just . p) <$> P.nonOptionPosArg
      po = Just <$> optParser
      ps = Nothing <$ P.stopper
      parser = po <|> ps <|> pa
  in catMaybes <$> P.manyTill parser P.end

-- | Provides information on each command that you wish to parse.
data Command a = Command
  { cmdName :: String
    -- ^ How the user identifies the command on the command line. For
    -- example, with @darcs@ this would be @get@, @send@, etc.

  , cmdIntersperse :: Intersperse
    -- ^ Each command may have options and positional arguments; may
    -- these be interspersed?

  , cmdOpts :: [C.OptSpec a]
    -- ^ Options for this command

  , cmdPosArgs :: String -> a
    -- ^ How to parse positional arguments for this command
  }

-- | Parses a command line that may feature options followed by a
-- command followed by more options and then followed by positional
-- arguments.
commands
  :: [C.OptSpec a]
     -- ^ Global options. These come after the program name but before
     -- the command name.
  -> ([a] -> Ex.Exceptional String b)
  -> (b -> Either (String -> c) [Command d])
  -> [String]
  -> Ex.Exceptional P.Error (b, Either [c] (String, [d]))
commands globals lsToB getCmds ss = P.parse ss $ do
  gs <- many $ C.parseOption globals
  b <- case lsToB gs of
    Ex.Exception e -> fail e
    Ex.Success g -> return g
  let cmds = getCmds b
  case cmds of
    Left fPa -> do
      posArgs <- (fmap (fmap fPa) $ many P.nextArg) <* P.end
      return (b, Left posArgs)
    Right cds -> do
      let cmdWords = Set.fromList . map cmdName $ cds
      (_, w) <- C.matchApproxWord cmdWords
      let cmd = fromJust . find (\c -> cmdName c == w) $ cds
          prsr = case cmdIntersperse cmd of
            Intersperse -> parseIntersperse
            StopOptions -> parseStopOpts
      rs <- prsr (C.parseOption (cmdOpts cmd))
            (cmdPosArgs cmd) <* P.end
      return (b, Right (w, rs))
