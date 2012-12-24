{-# LANGUAGE ExistentialQuantification #-}
-- | A simple command line parser that can parse options that take an
-- optional argument, one or two arguments, or a variable number of
-- arguments. For sample code that uses this parser, see
-- "System.Console.MultiArg.SampleParser".
module System.Console.MultiArg.SimpleParser (
  -- * Interspersion control
  Intersperse (Intersperse, StopOptions)

    -- * The parser
  , simple

    -- * Parsing multi-mode command lines
  , Mode(..)
  , modes
  ) where

import qualified System.Console.MultiArg.Prim as P
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
simple ::
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
simple i os p as =
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
  <*> many P.nextWord


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

-- | Provides information on each mode that you wish to parse.
data Mode result = forall b. Mode
  { mName :: String
    -- ^ How the user identifies the mode on the command line. For
    -- example, with @git@ this would be @commit@, @pull@, etc.

  , mIntersperse :: Intersperse
    -- ^ Each mode may have options and positional arguments; may
    -- these be interspersed?

  , mOpts :: [C.OptSpec b]
    -- ^ Options for this mode

  , mPosArgs :: String -> b
    -- ^ How to parse positional arguments for this mode

  , mProcess :: [b] -> result
    -- ^ Processes the options after they have been parsed.
  
  }

instance Functor Mode where
  fmap f (Mode nm i os pa p) =
    Mode nm i os pa (f . p)

processModeArgs :: Mode result -> P.Parser result
processModeArgs (Mode _ i os pa p) = do
  let prsr = case i of
        Intersperse -> parseIntersperse
        StopOptions -> parseStopOpts
  rs <- prsr (C.parseOption os) pa <* P.end
  return $ p rs

-- | Parses a command line that may feature options followed by a
-- mode followed by more options and then followed by positional
-- arguments.
modes
  :: [C.OptSpec a]
     -- ^ Global options. These come after the program name but before
     -- the mode name.

  -> ([a] -> Ex.Exceptional String b)
     -- ^ This function is applied to all the global options after
     -- they are parsed. To indicate a failure, return an Exception
     -- String; otherwise, return a successful value. This allows you
     -- to process the global options before the mode is parsed. (If
     -- you don't need to do any preprocessing, pass @return@ here.)
     -- If you indicate a failure here, parsing of the command line
     -- will stop and this error message will be returned.

  -> (b -> Either (String -> c) [Mode result])
     -- ^ This function determines whether modes will be parsed and,
     -- if so, which ones. The function is applied to the result of
     -- the pre-processing of the global options, so which modes are
     -- parsed and the behavior of those modes can vary depending on
     -- the global options. Return a Left to indicate that you do not
     -- want to parse modes at all. For instance, if the user passed a
     -- @--help@ option, you may not want to look for a mode after
     -- that. Otherwise, to parse modes, return a Right with a list of
     -- the modes.

  -> [String]
     -- ^ The command line to parse (presumably from 'getArgs')

  -> Ex.Exceptional P.Error (b, Either [c] result)
     -- ^ Returns an Exception if an error was encountered when
     -- parsing the command line (including if the global options
     -- procesor returned an Exception.) Otherwise, returns a
     -- pair. The first element of the pair is the result of the
     -- global options processor. The second element of the pair is an
     -- Either. It is Left if no modes were parsed, with a list of the
     -- positional arguments. It is a Right if modes were parsed, with
     -- the result of parsing the arguments to the mode.

modes globals lsToB getCmds ss = P.parse ss $ do
  gs <- P.manyTill (C.parseOption globals) endOrNonOpt
  b <- case lsToB gs of
    Ex.Exception e -> fail e
    Ex.Success g -> return g
  let cmds = getCmds b
  case cmds of
    Left fPa -> do
      posArgs <- (fmap (fmap fPa) $ many P.nextWord) <* P.end
      return (b, Left posArgs)
    Right cds -> do
      let cmdWords = Set.fromList . map mName $ cds
      (_, w) <- P.matchApproxWord cmdWords
      let cmd = fromJust . find (\c -> mName c == w) $ cds
      r <- processModeArgs cmd
      return (b, Right r)

-- | Looks at the next word. Succeeds if it is a non-option, or if we
-- are at the end of input. Fails otherwise.
endOrNonOpt :: P.Parser ()
endOrNonOpt = (P.lookAhead P.nonOptionPosArg >> return ())
              <|> P.end
