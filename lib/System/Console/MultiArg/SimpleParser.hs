{-# LANGUAGE ExistentialQuantification #-}
-- | Some pre-built command line parsers. One is a simple command line
-- parser that can parse options that take an optional argument, one
-- or two arguments, or a variable number of arguments. For sample
-- code that uses this parser, see
-- "System.Console.MultiArg.SampleParser".
--
-- Another parser is provided for multi-mode programs that are similar
-- to @git@ or @darcs@.
module System.Console.MultiArg.SimpleParser (
  -- * Interspersion control
  Intersperse (Intersperse, StopOptions)

    -- * The parser
  , simple
  , simpleWithHelp

    -- * Parsing multi-mode command lines
  , Mode(..)
  , modes
  , modesWithHelp
  ) where

import Data.Either (partitionEithers)
import qualified System.Console.MultiArg.Combinator as C
import qualified System.Console.MultiArg.GetArgs as GetArgs
import qualified System.Console.MultiArg.Prim as P
import qualified Control.Monad.Exception.Synchronous as Ex
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

-- | Parse a command line.
simple ::
  Intersperse
  -- ^ What to do after encountering the first positional argument

  -> [C.OptSpec a]
  -- ^ All possible options

  -> (String -> Ex.Exceptional C.InputError a)
  -- ^ How to handle positional arguments. This function is applied to
  -- the appropriate string every time the parser encounters a
  -- positional argument. This function shall return an Exception if
  -- something was wrong with the argument, or a Success if it is
  -- okay.

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


parsePosArg
  :: (String -> Ex.Exceptional C.InputError a)
  -> P.Parser a
parsePosArg p = do
  a <- P.nextWord
  case p a of
    Ex.Exception e ->
      let msg = "invalid positional argument: \"" ++ a ++ "\""
      in case e of
          C.NoMsg -> fail msg
          C.ErrorMsg s -> fail $ msg ++ ": " ++ s
    Ex.Success g -> return g

parseStopOpts
  :: P.Parser a
  -> (String -> Ex.Exceptional C.InputError a)
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
  -> (String -> Ex.Exceptional C.InputError a)
  -> P.Parser [a]
parseIntersperse optParser p =
  let pa = Just <$> parsePosArg p
      po = Just <$> optParser
      ps = Nothing <$ P.stopper
      parser = po <|> ps <|> pa
  in catMaybes <$> P.manyTill parser P.end

--
-- simpleWithHelp
--

-- | Wraps the OptSpec passed into to simpleWithHelp. 
data ArgWrap a
  = HelpArg
  | OtherArg a

data ArgWrapResult a
  = NeedsHelp String
  | NoHelp a

instance Functor ArgWrap where
  fmap _ HelpArg = HelpArg
  fmap f (OtherArg a) = OtherArg $ f a

partitionHelp :: [ArgWrap a] -> (DoHelp, [a])
partitionHelp xs = (not . null . fst $ r, snd r)
  where
    toEither h = case h of
      HelpArg -> Left ()
      OtherArg a -> Right a
    r = partitionEithers . map toEither $ xs

type ProgramName = String

-- | Parses a simple command line (that is, one without modes) in the
-- IO monad. Gets the arguments for you using 'getArgs'.  In addition
-- to the arguments you provide for 'simple', you also provide online
-- help. This function adds @-h@ and @--help@ options and shows help
-- if the user entered one of these options anywhere on the command
-- line. If help is shown, the program exits successfully. In
-- addition, it will print a message to standard error if parsing the
-- command line fails and then exit unsuccessfully.
simpleWithHelp
  :: (ProgramName -> String)
  -- ^ Help message. Printed as is, so it can be one line or have many
  -- lines. It should however have a final end-of-line character. The
  -- function is applied to the name of the program (which is
  -- retrieved at runtime.)

  -> Intersperse
  -- ^ What to do after encountering the first positional argument

  -> [C.OptSpec a]
  -- ^ All possible options. Do not add a @-h@ or @--help@ option;
  -- these are added for you.

  -> (String -> Ex.Exceptional C.InputError a)
  -- ^ How to handle positional arguments. This function is applied to
  -- the appropriate string every time the parser encounters a
  -- positional argument. This function shall return an Exception if
  -- something was wrong with the argument, or a Success if it is
  -- okay.

  -> IO [a]
  -- ^ If help is requested, the program will print it and exit
  -- successfully. If there was an error parsing the command line, the
  -- program will print an error message and exit
  -- unsuccessfully. Otherwise, the parsed arguments are returned.
simpleWithHelp h i os p =
  simpleWithShortCircuit [("h", ["help"], showHelp)] i os p showErr
  where
    showHelp = putStr . h
    showErr pn err = (C.formatError pn err)
      ++ "\nEnter \"" ++ pn ++ " -h\" for help.\n"


--
-- simpleWithShortCircuit
--

simpleWithShortCircuit
  :: [(String, [String], ProgramName -> IO void)]
  -> Intersperse
  -> [C.OptSpec a]
  -> (String -> Ex.Exceptional C.InputError a)
  -> (ProgramName -> P.Error -> String)
  -> IO [a]
simpleWithShortCircuit ss itr os posArg err = do
  let allOpts = map makeShortCircuitOpt ss
                ++ map (fmap Right) os
  as <- GetArgs.getArgs
  pn <- GetArgs.getProgName
  let exParsed = simple itr allOpts (fmap (fmap Right) posArg) as
  case exParsed of
    Ex.Exception e -> do
      IO.hPutStr IO.stderr $ err pn e
      exitFailure
    Ex.Success gs -> case partitionEithers gs of
      ([], xs) -> return xs
      (x:_, _) -> do { _ <- x pn; exitSuccess }

makeShortCircuitOpt
  :: (String, [String], String -> IO v)
  -> C.OptSpec (Either (String -> IO v) a)
makeShortCircuitOpt (ss, ls, act) = C.OptSpec ls ss (C.NoArg (Left act))

--
-- Mode parsing
--

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

  , mPosArgs :: String -> Ex.Exceptional C.InputError b
    -- ^ How to parse positional arguments for this mode. This
    -- function shall return an Exception if something was wrong with
    -- the argument, or a Success if it is okay.

  , mProcess :: [b] -> result
    -- ^ Processes the options after they have been parsed.

  , mHelp :: String -> String
    -- ^ Help string for this mode. This is used only in
    -- 'modesWithHelp'; 'modes' ignores this. This is displayed on
    -- screen exactly as is, so be sure to include the necessary
    -- trailing newline. The function is applied to the name of the
    -- program (which is retrieved at runtime.)
  
  }

instance Functor Mode where
  fmap f (Mode nm i os pa p h) =
    Mode nm i os pa (f . p) h

type DoHelp = Bool

modesWithHelpPure
  :: String
  -- ^ Program name
  -> (String -> String)
  -- ^ Global help
  -> [C.OptSpec a]
  -> ([a] -> Either ([String] -> result) [Mode result])
  -> [String]
  -> Ex.Exceptional P.Error (ArgWrapResult (ArgWrapResult result))
modesWithHelpPure pn getHlp globals lsToEi ss = P.parse ss $ do
  let globals' = addHelpOpt globals
  gs <- P.manyTill (C.parseOption globals') endOrNonOpt
  let (needsHelp, gblOs) = partitionHelp gs
  if needsHelp then return (NeedsHelp (getHlp pn)) else NoHelp <$>
    case lsToEi gblOs of
      Left parsePosArgs ->
            (NoHelp . parsePosArgs) <$> many P.nextWord <* P.end
      Right mds -> do
        let modeWords = Set.fromList . map mName $ mds
        (_, w) <- P.matchApproxWord modeWords
        let cmd = fromJust . find (\c -> mName c == w) $ mds
        processModeWithHelp pn cmd

modesNoHelp
  :: [C.OptSpec a]
  -> ([a] -> Either ([String] -> result) [Mode result])
  -> [String]
  -> Ex.Exceptional P.Error result
modesNoHelp globals lsToEi ss = P.parse ss $ do
  gs <- P.manyTill (C.parseOption globals) endOrNonOpt
  case lsToEi gs of
    Left parsePosArgs ->
          parsePosArgs <$> many P.nextWord <* P.end
    Right mds -> do
      let modeWords = Set.fromList . map mName $ mds
      (_, w) <- P.matchApproxWord modeWords
      let cmd = fromJust . find (\c -> mName c == w) $ mds
      processModeNoHelp cmd

processModeWithHelp
  :: String
  -- ^ Program name
   -> Mode result
   -> P.Parser (ArgWrapResult result)
processModeWithHelp pn (Mode _ i os pa p h) = do
  let prsr = case i of
        Intersperse -> parseIntersperse
        StopOptions -> parseStopOpts
      os' = addHelpOpt os
  rs <- prsr (C.parseOption os') (fmap (fmap OtherArg) pa) <* P.end
  let (needsHelp, parsedOpts) = partitionHelp rs
  return $ if needsHelp then NeedsHelp (h pn) else NoHelp $ p parsedOpts


processModeNoHelp
   :: Mode result
   -> P.Parser result
processModeNoHelp (Mode _ i os pa p _) = do
  let prsr = case i of
        Intersperse -> parseIntersperse
        StopOptions -> parseStopOpts
  rs <- prsr (C.parseOption os) pa <* P.end
  return $ p rs


addHelpOpt :: [C.OptSpec a] -> [C.OptSpec (ArgWrap a)]
addHelpOpt os =
  let helpOpt = C.OptSpec ["help"] "h" (C.NoArg HelpArg)
      origOpts = fmap (fmap OtherArg) os
  in helpOpt : origOpts

-- | Parses a command line that may feature options followed by a
-- mode followed by more options and then followed by positional
-- arguments.
modes
  :: [C.OptSpec a]
  -- ^ Global options. These come after the program name but before
  -- the mode name.

  -> ([a] -> Either ([String] -> result) [Mode result])
  -- ^ This function will be applied to the result of parsing the
  -- global options. The function must return a @Left@ if you do not
  -- want to parse any modes at all. This can be useful if one of the
  -- global options was something like @--help@ or @--version@ and so
  -- you do not need to see any mode. The function returned in the
  -- @Left@ will be applied to a list of all remaining command-line
  -- arguments after the global options.

  -> [String]
  -- ^ The command line to parse (presumably from 'getArgs')

  -> Ex.Exceptional P.Error result
  -- ^ Returns an Exception if an error was encountered when parsing
  -- any part of the command line (either the global options or the
  -- mode.) Otherwise, returns the result.
modes = modesNoHelp

-- | Like 'modes', but runs in the IO monad. Gets the command line
-- arguments for you.  This function adds the options @-h@ and
-- @--help@, both in the global options and in the options for each
-- mode. If @-h@ or @--help@ is entered in the global options, the
-- global help is shown and the program exits successfully; similarly,
-- if help is requested for a particular mode, that mode's help is
-- shown and the program exits successfully.
--
-- If an error occurs in the processing of the command line, an error
-- message is printed and the program exits with a failure.
modesWithHelp
  :: (String -> String)
  -- ^ Global help. This is a function that, when applied to the name
  -- of the program (which is retrieved at runtime), returns a help
  -- string. This is output exactly as is, so include any necessary
  -- trailing newlines.

  -> [C.OptSpec a]
  -- ^ Global options. These come after the program name but before
  -- the mode name. Do not add options for @-h@ or @--help@; these are
  -- added automatically.

  -> ([a] -> Either ([String] -> result) [Mode result])
  -- ^ This function will be applied to the result of parsing the
  -- global options. The function must return a @Left@ if you do not
  -- want to parse any modes at all. This can be useful if one of the
  -- global options was something like @--version@ and so
  -- you do not need to see any mode. The function returned in the
  -- @Left@ will be applied to a list of all remaining command-line
  -- arguments after the global options.

  -> IO result

modesWithHelp hlp glbls lsToEi = do
  as <- GetArgs.getArgs
  pn <- GetArgs.getProgName
  case modesWithHelpPure pn hlp glbls lsToEi as of
    Ex.Exception e -> do
      IO.hPutStr IO.stderr $ C.formatError pn e
      enterForHelp pn
      exitFailure
    Ex.Success g -> case g of
      NeedsHelp h -> putStr h >> exitSuccess
      NoHelp g2 -> case g2 of
        NeedsHelp h -> putStr h >> exitSuccess
        NoHelp g3 -> return g3

enterForHelp = undefined
-- | Looks at the next word. Succeeds if it is a non-option, or if we
-- are at the end of input. Fails otherwise.
endOrNonOpt :: P.Parser ()
endOrNonOpt = (P.lookAhead P.nonOptionPosArg >> return ())
              <|> P.end

--
--
--

data Opts a = Opts
  { oOptions :: [C.OptSpec a]
  , oShortcuts :: [(String, [String], ProgramName -> IO ())]
  }

data OptsWithPosArgs a = OptsWithPosArgs
  { opOpts :: Opts a
  , opIntersperse :: Intersperse
  , opPosArg :: String -> Ex.Exceptional C.InputError a
  }

data NMode g r = forall a. NMode
  { nmModeName :: String
  , nmGetResult :: [g] -> [a] -> r
  , nmOpts :: OptsWithPosArgs a
  }

makeShortcutOpt
  :: (String, [String], ProgramName -> IO v)
  -> C.OptSpec (Either (ProgramName -> IO v) a)
makeShortcutOpt (ss, ls, a) = C.OptSpec ls ss (C.NoArg (Left a))
                 
parseOpts :: Opts a -> P.Parser (Either (ProgramName -> IO ()) [a])
parseOpts os = do
  let specials = map makeShortcutOpt . oShortcuts $ os
      regs = map (fmap Right) . oOptions $ os
  eis <- P.manyTill (C.parseOption (specials ++ regs)) endOrNonOpt
  return $ case partitionEithers eis of
    (x:_, _) -> Left x
    ([], xs) -> Right xs

simplePure
  :: OptsWithPosArgs a
  -> Ex.Exceptional P.Error (Either (ProgramName -> IO v) [a])
simplePure = undefined

modesPure
  :: Opts a
  -- ^ Global options
  -> [NMode a r]
  -> Ex.Exceptional P.Error (Either (ProgramName -> IO v) r)
modesPure = undefined

simpleIO
  :: (ProgramName -> P.Error -> IO void)
  -> OptsWithPosArgs a
  -> IO [a]
simpleIO = undefined

modesIO
  :: (ProgramName -> P.Error -> IO void)
  -> Opts a
  -> [NMode a r]
  -> IO r
modesIO = undefined

simpleHelp
  :: (ProgramName -> String)
  -> OptsWithPosArgs a
  -> IO a
simpleHelp = undefined

simpleHelpVersion
  :: (ProgramName -> String)
  -> (ProgramName -> String)
  -> OptsWithPosArgs a
  -> IO a
simpleHelpVersion = undefined
