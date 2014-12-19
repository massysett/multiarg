module Multiarg.Internal where

import Multiarg.Maddash
import Multiarg.Limeline
import Multiarg.Types
import Multiarg.Util
import System.Environment
import System.Exit
import qualified System.IO as IO

limelineOutputToParsedCommandLine
  :: ([Either [Output a] (PosArg a)], Maybe OptName)
  -> ParsedCommandLine a
limelineOutputToParsedCommandLine (ls, mayOpt) =
  ParsedCommandLine (concatMap f ls) mayOpt
  where
    f ei = case ei of
      Left os -> map g os
        where
          g o = case o of
            Good a -> Right a
            OptionError oe -> Left oe
      Right (PosArg pa) -> [Right pa]


-- | Indicates the result of parsing a command line.
data ParsedCommandLine a = ParsedCommandLine
  { pclOutput :: [Either OptionError a]
  -- ^ A list of errors and results, in the original order in which
  -- they appeared on the command line.

  , pclInsufficientOptArgs :: Maybe OptName
  -- ^ 'Just' if the user included an option at the end of the command
  -- line and there were not enough following words to provide the
  -- option with its necessary arguments; otherwise 'Nothing'.
  } deriving (Eq, Ord, Show)

instance Functor ParsedCommandLine where
  fmap f (ParsedCommandLine ls m) = ParsedCommandLine
    (map (fmap f) ls) m

-- | Gets the results from a parsed command line.  If there were
-- errors, returns a 'Left' with an error message; otherwise, returns
-- a 'Right' with a list of the results.
parsedResults
  :: ParsedCommandLine a
  -> Either String [a]
parsedResults = undefined


parseCommandLine

  :: [OptSpec a]
  -- ^ All program options

  -> (String -> a)
  -- ^ Processes non-option positional arguments

  -> [String]
  -- ^ Input tokens from the command line, probably obtained from
  -- 'getArgs'

  -> ParsedCommandLine a

parseCommandLine os fPos inp = limelineOutputToParsedCommandLine limeOut
  where
    limeOut = interspersed shrts lngs fPos (map Token inp)
    (shrts, lngs) = splitOptSpecs os

parseCommandLineIO
  :: (String -> String)
  -- ^ Returns help for your command.  This function is applied to the
  -- name of the program being run, which is obtained from
  -- 'getProgName'.  The function should return a string that gives
  -- help for how to use your command; this string is printed as-is.

  -> [OptSpec a]
  -- ^ All program options.  An option for @-h@ and for @--help@ is
  -- added for you, using the help function given above.  If the user
  -- asks for help, then it is printed and the program exits
  -- successfully.  If the user gives a command line with one or more
  -- errors in it, an error message is printed, along with something
  -- like @Enter program-name --help for help@.

  -> (String -> a)
  -- ^ Processes positional arguments

  -> IO [a]
  -- ^ Fetches the command line arguments using 'getArgs' and
  -- processes them.  If there is an error, prints an error message
  -- and exits unsuccessfully.  Otherwise, returns the options parsed
  -- in.
parseCommandLineIO fHelp os fPos = do
  progName <- getProgName
  args <- getArgs
  case parsedResults $ parseCommandLineHelp os fPos args of
    Left err -> do
      IO.hPutStrLn IO.stderr $ progName ++ ": error"
      IO.hPutStr IO.stderr err
      IO.hPutStrLn IO.stderr $ "enter \"" ++ progName ++ " --help\" "
        ++ "for help."
      exitFailure
    Right mayResults -> case sequence mayResults of
      Nothing -> do
        putStr (fHelp progName)
        exitSuccess
      Just ls -> return ls


-- | Automatically adds a @-h@ and @--help@ option.  Intended
-- primarily for use by the 'parseCommandLineIO' function.
parseCommandLineHelp
  :: [OptSpec a]
  -> (String -> a)
  -> [String]
  -> ParsedCommandLine (Maybe a)
parseCommandLineHelp os fPos inp =
  limelineOutputToParsedCommandLine
  $ interspersed shrts lngs (fmap Just fPos) (map Token inp)
  where
    (shrts, lngs) = addHelpOption os

