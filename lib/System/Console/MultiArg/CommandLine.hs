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
  , ProgramName
  , Opts(..)
  , MapShortcuts(..)
  , OptsWithPosArgs(..)
  , NMode(..)


  -- * Simple parsers
  , simplePure
  , simpleIO
  , simpleHelp
  , simpleHelpVersion

  -- * Mode parsers
  , modesPure
  , modesIO

  ) where

import qualified System.Console.MultiArg.Combinator as C
import qualified System.Console.MultiArg.Prim as P
import qualified Control.Monad.Exception.Synchronous as Ex
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

-- | Looks at the next word. Succeeds if it is a non-option, or if we
-- are at the end of input. Fails otherwise.
endOrNonOpt :: P.Parser ()
endOrNonOpt = (P.lookAhead P.nonOptionPosArg >> return ())
              <|> P.end

--
--
--

data Opts s a = Opts
  { oOptions :: [C.OptSpec a]
  , oShortcuts :: [([String], String, C.ArgSpec s)]
  }

instance Functor (Opts s) where
  fmap f (Opts os ss) = Opts (map (fmap f) os) ss

class MapShortcuts f where
  smap :: (a -> b) -> f a o -> f b o

instance MapShortcuts Opts where
  smap f (Opts os ss) = Opts os (map g ss)
    where
      g (ls, s, as) = (ls, s, fmap f as)

data OptsWithPosArgs s a = OptsWithPosArgs
  { opOpts :: Opts s a
  , opIntersperse :: Intersperse
  , opPosArg :: String -> Ex.Exceptional C.InputError a
  }

instance MapShortcuts OptsWithPosArgs where
  smap f (OptsWithPosArgs os i p) = OptsWithPosArgs (smap f os) i p

instance Functor (OptsWithPosArgs s) where
  fmap f (OptsWithPosArgs os i p) =
    OptsWithPosArgs (fmap f os) i (fmap (fmap f) p)

data NMode g s r = forall a. NMode
  { nmModeName :: String
  , nmGetResult :: [g] -> [a] -> r
  , nmOpts :: OptsWithPosArgs s a
  }

instance MapShortcuts (NMode g) where
  smap f (NMode n g o) = NMode n g (smap f o)

instance Functor (NMode g s) where
  fmap f (NMode n gr os) = NMode n (\gs as -> f (gr gs as)) os

parseOpts :: Opts s a -> P.Parser (Either s [a])
parseOpts os = do
  let specials = map (\(ls, ss, a) -> C.OptSpec ls ss a)
                 . oShortcuts $ os
  maySpecial <- optional (C.parseOption specials <* P.end)
  case maySpecial of
    Nothing -> fmap Right
      $ P.manyTill (C.parseOption (oOptions os)) endOrNonOpt
    Just spec -> return . Left $ spec

parseOptsWithPosArgs
  :: OptsWithPosArgs s a
  -> P.Parser (Either s [a])
parseOptsWithPosArgs os = do
  let specials = map (\(ls, ss, a) -> C.OptSpec ls ss a)
                 . oShortcuts . opOpts $ os
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
  :: [g]
  -> [NMode g s r]
  -> P.Parser (Either s r)
parseModes gs ms = do
  let modeWords = Set.fromList . map nmModeName $ ms
  (_, w) <- P.matchApproxWord modeWords
  processMode (fromJust . find (\c -> nmModeName c == w) $ ms)
  where
    processMode (NMode _ gr os) = do
      eiOpts <- parseOptsWithPosArgs os
      return $ case eiOpts of
        Left x -> Left x
        Right opts -> Right (gr gs opts)


simplePure
  :: OptsWithPosArgs s a
  -> [String]
  -> Ex.Exceptional P.Error (Either s [a])
simplePure os ss = P.parse ss (parseOptsWithPosArgs os)

modesPure
  :: Opts s g
  -- ^ Global options
  -> [NMode g s r]
  -> [String]
  -> Ex.Exceptional P.Error (Either s r)
modesPure os ms ss = P.parse ss p
  where
    p = do
      eiGs <- parseOpts os
      case eiGs of
        Left spec -> return . Left $ spec
        Right gs -> parseModes gs ms

simpleIO
  :: [C.OptSpec a]
  -> Intersperse
  -> (String -> Ex.Exceptional C.InputError a)
  -> IO [a]
simpleIO os i getArg = do
  let optsWithArgs = OptsWithPosArgs (Opts os []) i getArg
  ss <- getArgs
  case simplePure optsWithArgs ss of
    Ex.Exception e -> errorAct e
    Ex.Success g -> case g of
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
    Ex.Exception e -> showErr e >> exitFailure
    Ex.Success g -> return g
  

modesIO
  :: (P.Error -> IO ())
  -> Opts s g
  -> [NMode g s r]
  -> IO (Either s r)
modesIO showErr os ms = do
  ss <- getArgs
  case modesPure os ms ss of
    Ex.Exception e -> showErr e >> exitFailure
    Ex.Success g -> return g


type ProgramName = String

displayAct :: (ProgramName -> String) -> IO a
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

simpleHelp
  :: (ProgramName -> String)
  -> [C.OptSpec a]
  -> Intersperse
  -> (String -> Ex.Exceptional C.InputError a)
  -> IO [a]
simpleHelp getHelp os ir getArg = do
  let shortcuts = [(["help"], "h", C.NoArg (displayAct getHelp))]
      opts = OptsWithPosArgs (Opts os shortcuts) ir getArg
  ei <- simpleIOCustomError errorActDisplayHelp opts
  case ei of
    Left act -> act
    Right as -> return as

simpleHelpVersion
  :: (ProgramName -> String)
  -- ^ Generates help string
  -> (ProgramName -> String)
  -- ^ Generates version string
  -> [C.OptSpec a]
  -> Intersperse
  -> (String -> Ex.Exceptional C.InputError a)
  -> IO [a]
simpleHelpVersion getHelp getVer os ir getArg = do
  let shortcuts = [ (["help"], "h", C.NoArg (displayAct getHelp))
                  , (["version"], "v", C.NoArg (displayAct getVer)) ]
      opts = OptsWithPosArgs (Opts os shortcuts) ir getArg
  ei <- simpleIOCustomError errorActDisplayHelp opts
  case ei of
    Left act -> act
    Right as -> return as

