{-# LANGUAGE ExistentialQuantification #-}
-- | Some pre-built command line parsers. One is a simple command line
-- parser that can parse options that take an optional argument, one
-- or two arguments, or a variable number of arguments. For sample
-- code that uses this parser, see
-- "System.Console.MultiArg.SampleParser".
--
-- Another parser is provided for multi-mode programs that are similar
-- to @git@ or @darcs@.
--
-- All the parsers in this module allow the user to intersperse
-- positional arguments and options.  For example, in the command line
-- @ls -l myfile -h myfile2@, both @-l@ and @-h@ will be interpreted
-- as options.  If the user supplies a word that begins with a hyphen
-- but it is not a valid option, an error occurs.  If the user wants
-- to indicate that all remaining words on the command line are
-- positional arguments, he can use a stopper.  For example, in the
-- command line @ls -l myfile -- -h myfile2@, @myfile@, @-h@, and
-- @myfile2@ are interpreted as positional arguments.
--
-- Previously there was a bug in System.Environment.getArgs that would
-- not properly encode Unicode command line arguments.  multiarg used
-- to provide its own GetArgs module to deal with this.  This bug was
-- in base 4.3.1.0, which was bundled with ghc 7.0.4.  This bug was
-- fixed in base 4.4.0.0, which came with ghc 7.2.  Since this bug has
-- been fixed for awhile, multiarg no longer has its own GetArgs
-- module.
module System.Console.MultiArg.CommandLine (

  -- * Types
    ProgName
  , Opts(..)
  , MapShortcuts(..)
  , OptsWithPosArgs(..)
  , Mode(..)

  -- * Simple parsers
  , simplePure
  , simpleIO
  , simpleHelp
  , simpleHelpVersion

  -- * Mode parsers
  , modesPure
  , modesIO

  -- * Helpers to create various options and modes
  , optsHelp
  , optsHelpVersion
  , modeHelp

  ) where

import qualified System.Console.MultiArg.Combinator as C
import qualified System.Console.MultiArg.Prim as P
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import qualified System.IO as IO
import Control.Applicative ( (<|>), optional,
                             (<$), (<*), (<$>))
import Data.List (find)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set


-- | Specifies a set of options.
data Opts s a = Opts
  { oOptions :: [C.OptSpec a]
  -- ^ If the user does not specify any shortcut options, she may
  -- specify any number of these options.

  , oShortcuts :: [C.OptSpec s]
  -- ^ Shortcut options are commonly options such as @--help@ or
  -- @--version@. Such options must be specified alone on the command
  -- line.  The parser looks for one of these options first.  If it
  -- finds one and it is the only option on the command line, only
  -- this option is processed and returned.  If the option is not
  -- alone on the command line, an error occurs.  If no shortcut
  -- option is found, the parser processes non-shortcut options
  -- instead.

  }

-- | Creates an Opts with a help shortcut option.
optsHelp
  :: h
  -- ^ Whatever type you wish to use for help
  -> [C.OptSpec a]
  -> Opts h a
optsHelp h os = Opts os [C.OptSpec ["help"] "h" (C.NoArg h)]

-- | Creates an Opts with help and version shortcut options.
optsHelpVersion
  :: h
  -- ^ What you wish to use for help

  -> h
  -- ^ What you wish to use for version

  -> [C.OptSpec a]
  -> Opts h a
optsHelpVersion h v os = Opts os [ C.OptSpec ["help"] "h" (C.NoArg h)
                                 , C.OptSpec ["version"] "v" (C.NoArg v) ]

instance Functor (Opts s) where
  fmap f (Opts os ss) = Opts (map (fmap f) os) ss

-- | Things that contain shortcut options that can be changed.
class MapShortcuts f where
  smap :: (a -> b) -> f a o -> f b o

instance MapShortcuts Opts where
  smap f (Opts os ss) = Opts os (map (fmap f) ss)

-- | Specification for both options and positional arguments.
data OptsWithPosArgs s a = OptsWithPosArgs
  { opOpts :: Opts s a
  , opPosArg :: String -> Either C.InputError a
  }

instance MapShortcuts OptsWithPosArgs where
  smap f (OptsWithPosArgs os p) = OptsWithPosArgs (smap f os) p

instance Functor (OptsWithPosArgs s) where
  fmap f (OptsWithPosArgs os p) =
    OptsWithPosArgs (fmap f os) (fmap (fmap f) p)

-- | Specifies a mode.
data Mode s r = forall a. Mode
  { mModeName :: String
  -- ^ How the user specifies the mode on the command line.  For @git@
  -- for example this might be @commit@ or @log@.

  , mGetResult :: [a] -> r
  -- ^ This function is applied to a list of the results of parsing the
  -- options that are specific to this mode.  The function returns a
  -- type of your choosing (though all modes in the same parser will
  -- have to return the same type.)

  , mOpts :: OptsWithPosArgs s a
  -- ^ Options and positional arguments that are specific to this
  -- mode.  For example, in the command line @git commit -a -m 'this
  -- is a log message'@, @commit@ is the mode name and everything
  -- after that is specified here as an option or positional argument
  -- that is specific to this mode.
  }

-- | Creates a Mode with a help option (help specific to the mode.)
modeHelp
  :: String
  -- ^ Mode name

  -> h
  -- ^ Whatever you want to use for the help (perhaps a string, or a
  -- function, or an IO action).  Its type will have to match up with
  -- the type of the global shortcut options and with the shortcut
  -- type of the other modes.

  -> ([a] -> r)
  -- ^ When applied to the the mode options, returns the result.

  -> [C.OptSpec a]
  -- ^ Options for this mode

  -> (String -> Either C.InputError a)
  -- ^ Parses positional arguments

  -> Mode h r

modeHelp n h getR os p =
  Mode n getR (OptsWithPosArgs (Opts os ss) p)
  where
    ss = [C.OptSpec ["help"] "h" (C.NoArg h)]

instance MapShortcuts Mode where
  smap f (Mode n g o) = Mode n g (smap f o)

instance Functor (Mode s) where
  fmap f (Mode n gr os) = Mode n (fmap f gr) os

parseOpts :: Opts s a -> P.Parser (Either s [a])
parseOpts os = do
  let specials = oShortcuts os
  maySpecial <- optional (C.parseOption specials <* P.end)
  case maySpecial of
    Nothing -> fmap Right
      $ P.manyTill (C.parseOption (oOptions os)) endOrNonOpt
    Just spec -> return . Left $ spec

parseOptsWithPosArgs
  :: OptsWithPosArgs s a
  -> P.Parser (Either s [a])
parseOptsWithPosArgs os = do
  let specials = oShortcuts . opOpts $ os
  maySpecial <- optional (C.parseOption specials <* P.end)
  case maySpecial of
    Nothing ->
      let parser = C.parseOption (oOptions . opOpts $ os)
      in fmap Right $ parseIntersperse parser (opPosArg os)
    Just spec -> return . Left $ spec

parseModes
  :: [Mode s r]
  -> P.Parser (Either s r)
parseModes ms = do
  let modeWords = Set.fromList . map mModeName $ ms
  (_, w) <- P.matchApproxWord modeWords
  processMode (fromJust . find (\c -> mModeName c == w) $ ms)
  where
    processMode (Mode _ gr os) = do
      eiOpts <- parseOptsWithPosArgs os
      return $ case eiOpts of
        Left x -> Left x
        Right opts -> Right (gr opts)


-- | A pure (non-IO) parser for simple command lines--that is, command
-- lines that do not have modes.
simplePure
  :: OptsWithPosArgs s a
  -- ^ Specifies allowed regular options, allowed shortcut options,
  -- and how to parse positional arguments.  Also specifies whether
  -- the user may intersperse options with positional arguments.

  -> [String]
  -- ^ The command line arguments to parse

  -> Either P.Error (Either s [a])
  -- ^ Returns an error if the command line arguments could not be
  -- parsed. If the parse was successful, returns an Either.  A Left
  -- indicates that the user selected a shortcut option.  A Right
  -- indicates that the user did not specify a shortcut option, and
  -- will contain a list of the options and positional arguments.
simplePure os ss = P.parse ss (parseOptsWithPosArgs os)

-- | A pure (non-IO) parser for command lines that contain modes.
modesPure
  :: Opts s g
  -- ^ Global options.  These are specified before any mode.  For
  -- instance, in the command @git --no-pager commit -a@, the option
  -- @--no-pager@ is a global option.  Global options can contain
  -- shortcut options.  For instance, @git --help@ contains a single
  -- shortcut option.

  -> ([g] -> Either String (Either r [Mode s r]))
  -- ^ This function processes the global options.  If there are no
  -- shortcut options specified in the global options, it is applied
  -- to the result of processing the global options.  This function
  -- may return an Exception if there is something wrong with the
  -- global options (a nonsensical combination, perhaps.)  Otherwise,
  -- it returns an Either.  Return a Left if there is no need to
  -- process any modes at all after seeing the global options.
  -- Otherwise, return a Right with a list of modes.

  -> [String]
  -- ^ Command line arguments to parse

  -> Either P.Error (Either s r)
  -- ^ If the command line arguments fail to parse, this will be an
  -- Exception with the error.  If the parser is successful, this
  -- returns an Either. A Left indicates that the user entered a
  -- shortcut option, either in the global options or in one of the
  -- mode-specific options.  A Right indicates that the user selected
  -- a mode.
modesPure os process ss = P.parse ss p
  where
    p = do
      eiGs <- parseOpts os
      case eiGs of
        Left spec -> return . Left $ spec
        Right gs -> case process gs of
          Left s -> fail s
          Right eiModes -> case eiModes of
            Left r -> return (Right r)
            Right modes -> parseModes modes

-- | A parser for simple command lines that do not contain modes.
-- Runs in the IO monad.
simpleIO
  :: [C.OptSpec a]
  -- ^ Options to parse

  -> (String -> Either C.InputError a)
  -- ^ How to parse positional arguments

  -> IO [a]
  -- ^ If there is an error parsing the command line, the program will
  -- exit with an error message.  If successful the results are
  -- returned here.
simpleIO os getArg = do
  let optsWithArgs = OptsWithPosArgs (Opts os []) getArg
  ss <- getArgs
  case simplePure optsWithArgs ss of
    Left e -> errorAct e
    Right g -> case g of
      Left _ ->
        error "simpleIO: should never happen: no shortcut options"
      Right gs -> return gs

simpleIOCustomError
  :: (P.Error -> IO ())
  -> OptsWithPosArgs s a
  -> IO (Either s [a])
simpleIOCustomError showErr os = do
  ss <- getArgs
  case simplePure os ss of
    Left e -> showErr e >> exitFailure
    Right g -> return g
  

-- | A command line parser for multi-mode command lines.  Runs in the
-- IO monad.
modesIO
  :: Opts s g
  -- ^ Specifies global options and global shortcut options

  -> ([g] -> Either String (Either r [Mode s r]))
  -- ^ This function processes the global options.  If there are no
  -- shortcut options specified in the global options, it is applied
  -- to the result of processing the global options.  This function
  -- may return an Exception if there is something wrong with the
  -- global options (a nonsensical combination, perhaps.)  Otherwise,
  -- it returns an Either.  Return a Left if there is no need to
  -- process any modes at all after seeing the global options.
  -- Otherwise, return a Right with a list of modes.

  -> IO (Either s r)
  -- ^ If parsing fails, the program will exit with a failure. If
  -- successful, the result is returned here.  A Left indicates a
  -- shortcut option, either from the global options or from the
  -- mode-specific options; a Right indicates the mode a user
  -- selected.
modesIO os ms = do
  ss <- getArgs
  case modesPure os ms ss of
    Left e -> errorAct e
    Right g -> return g


-- | The name of the program that was entered on the command line,
-- obtained from System.Environment.getProgName.
type ProgName = String

displayAct :: (ProgName -> String) -> IO a
displayAct getHelp = do
  pn <- getProgName
  putStr $ getHelp pn
  exitSuccess

errorAct :: P.Error -> IO a
errorAct e = do
  pn <- getProgName
  IO.hPutStr IO.stderr $ C.formatError pn e
  exitFailure

errorActDisplayHelp :: P.Error -> IO a
errorActDisplayHelp e = do
  pn <- getProgName
  IO.hPutStr IO.stderr $ C.formatError pn e
  IO.hPutStrLn IO.stderr $ "enter \"" ++ pn ++ " -h\" for help."
  exitFailure

-- | A parser for simple command lines. Adds a @--help@ option for
-- you.
simpleHelp
  :: (ProgName -> String)
  -- ^ Indicate the help here. This function, when applied to the name
  -- of the program, returns help.  simpleHelp automatically adds
  -- options for @--help@ and @-h@ for you.

  -> [C.OptSpec a]
  -- ^ Options to parse

  -> (String -> Either C.InputError a)
  -- ^ How to parse positional arguments

  -> IO [a]
  -- ^ If the parser fails, the program will exit with an error.  If
  -- the user requested help, it will be displayed and the program
  -- exits successfully.  Otherwise, the options and positional
  -- arguments are returned here.
simpleHelp getHelp os getArg = do
  let shortcuts = [C.OptSpec ["help"] "h" (C.NoArg (displayAct getHelp))]
      opts = OptsWithPosArgs (Opts os shortcuts) getArg
  ei <- simpleIOCustomError errorActDisplayHelp opts
  case ei of
    Left act -> act
    Right as -> return as

-- | A parser for simple command lines without modes.  Adds options
-- for @--help@ and @--version@ for you.
simpleHelpVersion
  :: (ProgName -> String)
  -- ^ Indicate the help here. This function, when applied to the name
  -- of the program, returns help.  simpleHelpVersion automatically adds
  -- options for @--help@ and @-h@ for you.

  -> (ProgName -> String)
  -- ^ Indicate the version here. This function, when applied to the
  -- name of the program, returns a version string.  simpleHelpVersion
  -- automatically adds an option for @--version@ for you.

  -> [C.OptSpec a]
  -- ^ Options to parse

  -> (String -> Either C.InputError a)
  -- ^ How to parse positional arguments

  -> IO [a]
  -- ^ If the parser fails, the program will exit with an error.  If
  -- the user requested help or version information, it will be
  -- displayed and the program exits successfully.  Otherwise, the
  -- options and positional arguments are returned here.

simpleHelpVersion getHelp getVer os getArg = do
  let shortcuts = [ C.OptSpec ["help"] "h"
                      (C.NoArg (displayAct getHelp))
                  , C.OptSpec ["version"] ""
                      (C.NoArg (displayAct getVer)) ]
      opts = OptsWithPosArgs (Opts os shortcuts) getArg
  ei <- simpleIOCustomError errorActDisplayHelp opts
  case ei of
    Left act -> act
    Right as -> return as

-- # Helpers

-- | Handles positional arguments and errors with them.
parsePosArg
  :: (String -> Either C.InputError a)
  -- ^ Function to handle positional arguments
  -> P.Parser a
parsePosArg f = do
  a <- P.nonOptionPosArg
  case f a of
    Left e ->
      let msg = "invalid positional argument: \"" ++ a ++ "\""
      in case e of
          C.NoMsg -> fail msg
          C.ErrorMsg s -> fail $ msg ++ ": " ++ s
    Right g -> return g

-- | @parseIntersperse o p@ parses options and positional arguments,
-- where o is a parser that parses options, and p is a function that,
-- when applied to a string, returns the appropriate type.
--
-- If a stopper has not yet been seen, any word that begins with a
-- hyphen will not be parsed as a positional argument.  Therefore, if
-- there is a word before a stopper and it begins with a hyphen, if it
-- is not a valid option then the parse will fail with an error.
parseIntersperse
  :: P.Parser a
  -> (String -> Either C.InputError a)
  -> P.Parser [a]
parseIntersperse optParser p =
  let pa = Just <$> parsePosArg p
      po = Just <$> optParser
      ps = Nothing <$ P.stopper
      parser = po <|> ps <|> pa
  in catMaybes <$> P.manyTill parser P.end

-- | Looks at the next word. Succeeds if it is a non-option, or if we
-- are at the end of input. Fails otherwise.
endOrNonOpt :: P.Parser ()
endOrNonOpt = (P.lookAhead P.nonOptionPosArg >> return ())
              <|> P.end

