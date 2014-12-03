-- | A combinator library for building command-line parsers.
-- This parser was built because I could not find anything that would
-- readily parse command lines where the options took more than one
-- argument. For example, for the @tail@ command on GNU systems, the
-- @--lines@ option takes one argument to specify how many lines you
-- want to see. Well, what if you want to build a program with an
-- option that takes /two/ arguments, like @--foo bar baz@? I found no
-- such library so I built this one. Nevertheless, using this library
-- you can build parsers to parse a variety of command line
-- vocabularies, from simple to complex.
--
-- Some terms are used throughout multiarg:
--
-- [@word@] When you run your program from the Unix shell prompt,
-- your shell is responsible for splitting the command line into
-- words. Typically you separate words with spaces, although quoting
-- can affect this. multiarg parses lists of words. Each word can
-- consist of a single long option, a single long option and an
-- accompanying option argument, a single short option, multiple
-- short options, and even one or more multiple short options and an
-- accompanying short option argument. Or, a word can be a
-- positional argument or a stopper. All these are described below.
--
-- [@option@] Options allow a user to specify ways to tune the
-- operation of a program. Typically options are indeed optional,
-- although some programs do sport \"required options\" (a bit of an
-- oxymoron). Options can be either short options or long
-- options. Also, options can take arguments.
--
-- [@short option@] An option that is specified with a single hyphen
-- and a single letter. For example, for the program @tail(1)@,
-- possible short options include @n@ and @v@. With multiarg it is
-- possible to easily parse short options that are specified in
-- different words or in the same word. For example, if a user wants
-- to run @tail@ with two options, he might type @tail -v -f@ or he
-- might type @tail -vf@.
--
-- [@long option@] An option that is specified using two hyphens and
-- what is usually a mnemonic word, though it could be as short as a
-- single letter. For example, @tail(1)@ has long options including
-- @follow@ and @verbose@. The user would specify these on the
-- command line by typing @tail --follow --verbose@.
--
-- [@option argument@] Some options take additional arguments that
-- are specific to the option and change what the option does. For
-- instance, the @lines@ option to @tail(1)@ takes a single,
-- optional argument, which is the number of lines to show. Option
-- arguments can be optional or required, and a single option can
-- take a mulitple, fixed number of arguments and it can take a
-- variable number of arguments. Option arguments can be given in
-- various ways. They can be specified in the same word as a long
-- option by using an equals sign; they can also be specified in the
-- same word as a short option simply by placing them in the same
-- word, or they can be specified in the following word. For
-- example, these different command lines all mean the same thing;
-- @tail --verbose --lines=20@, @tail --verbose --lines 20@, @tail
-- -vn 20@, @tail -v -n20@, @tail -vn20@, and @tail -v -n 20@, and
-- numerous other combinations also have the same meaning.
--
-- [@GNU-style option argument@] A long option with an argument
-- given with an equal sign, such as [@lines=20@].
--
-- [@positional argument@] A word on the command line that is not an
-- option or an argument to an option. For instance, with @tail(1)@,
-- you specify the files you want to see by using positional
-- arguments. In the command @tail -n 10 myfile@, @myfile@ is a
-- positional argument. For some programs, such as @git@ or @darcs@,
-- a positional argument might be a \"command\" or a \"mode\", such
-- as the @commit@ in @git commit@ or the @whatsnew@ in @darcs
-- whatsnew@. multiarg has no primitive parsers that treat these
-- positional arguments specially but it is trivial to build a
-- parser for command lines such as this, too.
--
-- [@stopper@] A single word consisting solely of two hyphens,
-- @--@. The user types this to indicate that all subsequent words
-- on the command line are positional arguments, even if they begin
-- with hyphens and therefore look like they might be options.
--
-- [@pending@] The user might specify more than one short option, or
-- a short option and a short option argument, in a single word. For
-- example, she might type @tail -vl20@. After parsing the @v@
-- option, the Parser makes @l20@ into a \"pending\". The next
-- parser can then treat @l20@ as an option argument to the @v@
-- option (which is probably not what was wanted) or the next parser
-- can parse @l@ as a short option. This would result in a
-- \"pending\" of @20@. Then, the next parser can treat @20@ as an
-- option argument. After that parse there will be no pendings.

module Multiarg
  ( ArgSpec(..)
  , OptSpec(..)
  , Intersperse(..)
  , parseCommandLine
  , parseCommandLineIO
  , parseCommandLineHelp

  -- * Errors
  , Short(..)
  , Long(..)
  , Option(..)
  , OptionError(..)
  , CommandLineError(..)
  , prettyCommandLineError
  ) where

import Multiarg.Maddash
import Multiarg.Limeline
import Multiarg.Types
import Multiarg.Util
import System.Environment
import System.Exit
import qualified System.IO as IO


data CommandLineError = CommandLineError
  { cleFirst :: [OptionError]
  , cleLast :: Either OptionError Option
  } deriving (Eq, Ord, Show)

limelineOutputToCommandLineError
  :: ([Either [Output a] (PosArg a)], Maybe Option)
  -> Either CommandLineError [a]

limelineOutputToCommandLineError (ls, mayOpt) =
  case (mayLast outErrors, mayOpt) of
    (Nothing, Nothing) -> Right goods
    (Just (xs, x), Nothing) -> Left (CommandLineError xs (Left x))
    (Nothing, Just err) -> Left (CommandLineError [] (Right err))
    (Just (xs, x), Just err) -> Left (CommandLineError (xs ++ [x])
                                                       (Right err))
    where
      (outErrors, goods) = foldr f ([], []) ls
        where
          f ei (ers, gds) = case ei of
            Left outs -> foldr g (ers, gds) outs
              where
                g out (es, gs) = case out of
                  Good gd -> (es, gd : gs)
                  OptionError e -> (e : es, gs)
            Right (PosArg g) -> (ers, g : gds)

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
  deriving (Eq, Ord, Show)

parseCommandLine

  :: Intersperse

  -> [OptSpec a]
  -- ^ All program options

  -> (String -> a)
  -- ^ Processes non-option positional arguments

  -> [String]
  -- ^ Input tokens from the command line, probably obtained from
  -- 'getArgs'

  -> Either CommandLineError [a]
  -- ^ If there were one or more errors when parsing the command line,
  -- these errors are returned; otherwise, returns the parsed result,
  -- in the same order in which it appeared on the command line.

parseCommandLine int os fPos inp = limelineOutputToCommandLineError limeOut
  where
    limeOut = fLime shrts lngs fPos (map Token inp)
    (shrts, lngs) = splitOptSpecs os
    fLime = case int of
      Intersperse -> interspersed
      StopOptions -> nonInterspersed

parseCommandLineIO
  :: (String -> String)
  -- ^ Returns help for your command.  This function is applied to the
  -- name of the program being run, which is obtained from
  -- 'getProgName'.  The function should return a string that gives
  -- help for how to use your command; this string is printed as-is.

  -> Intersperse

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
parseCommandLineIO fHelp int os fPos = do
  progName <- getProgName
  args <- getArgs
  case parseCommandLineHelp int os fPos args of
    Left err -> do
      IO.hPutStr IO.stderr (prettyCommandLineError progName err)
      exitFailure
    Right mayLs -> case mayLs of
      Nothing -> do
        putStr (fHelp progName)
        exitSuccess
      Just ls -> return ls


-- | Automatically adds a @-h@ and @--help@ option.  Intended
-- primarily for use by the 'parseCommandLineIO' function.
parseCommandLineHelp
  :: Intersperse
  -> [OptSpec a]
  -> (String -> a)
  -> [String]
  -> Either CommandLineError (Maybe [a])
  -- ^ Returns an error if applicable; otherwise, returns a Maybe.
  -- The Maybe is Nothing if the user asked for help; otherwise, the
  -- parsed output is returned.
parseCommandLineHelp int os fPos inp = fmap parsedOpts parsed
  where
    limeOut = fLime shrts lngs (fmap Right fPos) (map Token inp)
    (shrts, lngs) = addHelpOption os
    fLime = case int of
      Intersperse -> interspersed
      StopOptions -> nonInterspersed
    parsed = limelineOutputToCommandLineError limeOut


-- | Shows errors in a pretty way.
prettyCommandLineError :: String -> CommandLineError -> String
prettyCommandLineError = undefined
