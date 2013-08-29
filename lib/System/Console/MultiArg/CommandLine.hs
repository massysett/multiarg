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
-- Previously there was a bug in System.Environment.getArgs that would
-- not properly encode Unicode command line arguments.  multiarg used
-- to provide its own GetArgs module to deal with this.  This bug was
-- in base 4.3.1.0, which was bundled with ghc 7.0.4.  This bug was
-- fixed in base 4.4.0.0, which came with ghc 7.2.  Since this bug has
-- been fixed for awhile, multiarg no longer has its own GetArgs
-- module.
module System.Console.MultiArg.CommandLine (
  -- * Interspersion control
  Intersperse (Intersperse, StopOptions)

  -- * Types
  , ProgName
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
  , opIntersperse :: Intersperse
  , opPosArg :: String -> Either C.InputError a
  }

instance MapShortcuts OptsWithPosArgs where
  smap f (OptsWithPosArgs os i p) = OptsWithPosArgs (smap f os) i p

instance Functor (OptsWithPosArgs s) where
  fmap f (OptsWithPosArgs os i p) =
    OptsWithPosArgs (fmap f os) i (fmap (fmap f) p)

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

  -> Intersperse
  -- ^ Allow interspersion of mode options and positional arguments?

  -> (String -> Either C.InputError a)
  -- ^ Parses positional arguments

  -> Mode h r

modeHelp n h getR os i p =
  Mode n getR (OptsWithPosArgs (Opts os ss) i p)
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
      let f = case opIntersperse os of
            Intersperse -> parseIntersperse
            StopOptions -> parseStopOpts
          parser = C.parseOption (oOptions . opOpts $ os)
      in fmap Right $ f parser (opPosArg os)
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

  -> Intersperse
  -- ^ Allow interspersion of options and arguments?

  -> (String -> Either C.InputError a)
  -- ^ How to parse positional arguments

  -> IO [a]
  -- ^ If there is an error parsing the command line, the program will
  -- exit with an error message.  If successful the results are
  -- returned here.
simpleIO os i getArg = do
  let optsWithArgs = OptsWithPosArgs (Opts os []) i getArg
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

  -> Intersperse
  -- ^ Allow interspersion of options and positional arguments?

  -> (String -> Either C.InputError a)
  -- ^ How to parse positional arguments

  -> IO [a]
  -- ^ If the parser fails, the program will exit with an error.  If
  -- the user requested help, it will be displayed and the program
  -- exits successfully.  Otherwise, the options and positional
  -- arguments are returned here.
simpleHelp getHelp os ir getArg = do
  let shortcuts = [C.OptSpec ["help"] "h" (C.NoArg (displayAct getHelp))]
      opts = OptsWithPosArgs (Opts os shortcuts) ir getArg
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

  -> Intersperse
  -- ^ Allow interspersion of options and positional arguments?

  -> (String -> Either C.InputError a)
  -- ^ How to parse positional arguments

  -> IO [a]
  -- ^ If the parser fails, the program will exit with an error.  If
  -- the user requested help or version information, it will be
  -- displayed and the program exits successfully.  Otherwise, the
  -- options and positional arguments are returned here.

simpleHelpVersion getHelp getVer os ir getArg = do
  let shortcuts = [ C.OptSpec ["help"] "h"
                      (C.NoArg (displayAct getHelp))
                  , C.OptSpec ["version"] ""
                      (C.NoArg (displayAct getVer)) ]
      opts = OptsWithPosArgs (Opts os shortcuts) ir getArg
  ei <- simpleIOCustomError errorActDisplayHelp opts
  case ei of
    Left act -> act
    Right as -> return as

-- # Helpers

-- | Parses positional arguments and handles errors with them.
parsePosArg
  :: (String -> Either C.InputError a)
  -> P.Parser a
parsePosArg p = do
  a <- P.nextWord
  case p a of
    Left e ->
      let msg = "invalid positional argument: \"" ++ a ++ "\""
      in case e of
          C.NoMsg -> fail msg
          C.ErrorMsg s -> fail $ msg ++ ": " ++ s
    Right g -> return g

-- | Parses options only, where they are not interspersed with
-- positional arguments.  Stops parsing only where it encouters a word
-- that does not begin with a dash.  This way if the user enters a bad
-- option, it shows in the error message as a bad option rather than
-- simply not getting parsed.
parseOptsNoIntersperse :: P.Parser a -> P.Parser [a]
parseOptsNoIntersperse p = P.manyTill p e where
  e = P.end <|> nonOpt
  nonOpt = P.lookAhead next
  next = (() <$ P.nonOptionPosArg) <|> P.stopper

-- | Parses options and positional arguments where the two are not
-- interspersed. Stops parsing options when a stopper is encountered
-- or at the first word that does not look like an option.
parseStopOpts
  :: P.Parser a
  -> (String -> Either C.InputError a)
  -> P.Parser [a]
parseStopOpts optParser p =
  (++)
  <$> parseOptsNoIntersperse optParser
  <* optional P.stopper
  <*> many (parsePosArg p)


-- | @parseIntersperse o p@ parses options and positional arguments,
-- where o is a parser that parses options, and p is a function that,
-- when applied to a string, returns the appropriate type.
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

