-- | Helps you build command-line parsers for programs that have more
-- than one so-called /mode/; examples of such programs include @git@,
-- @darcs@, and @ghc-pkg@.
module Multiarg.Mode
  ( -- * Creating a single 'Mode'
    ArgSpec(..)
  , OptSpec(..)
  , optSpec
  , ModeName(..)
  , Mode(..)
  , mode

  -- * Parsing
  , parseModeLine

  -- * Errors
  , ShortName(..)
  , LongName(..)
  , OptName(..)
  , OptionError(..)
  , CommandLineError(..)
  , LastGlobalError(..)
  , ModeError(..)
  , GlobalError(..)
  , ModeCommandError(..)
  , ModelineError(..)
  ) where

import Data.List (isPrefixOf)
import Data.Either (partitionEithers)
import Multiarg.Maddash
import Multiarg
import Multiarg.Util

data LastGlobalError
  = LastWordError (Either OptName Token)
  -- ^ A Left indicates that a bad option was passed.  A Right
  -- indicates that the last word was not an option, but was not
  -- recognized as a valid mode.
  | LastGlobalOptsError OptionError
  deriving (Eq, Ord, Show)

data GlobalError = GlobalError [OptionError] LastGlobalError
  deriving (Eq, Ord, Show)

data ModeCommandError = ModeCommandError ModeName CommandLineError
  deriving (Eq, Ord, Show)

data ModelineError
  = GlobalAndLocal GlobalError ModeCommandError
  | GlobalOrLocal (Either GlobalError ModeCommandError)
  deriving (Eq, Ord, Show)

newtype ModeName = ModeName String
  deriving (Eq, Ord, Show)

data Mode a = Mode
  { modeToModeName :: ModeName
  , modeToParser :: [Token] -> Either CommandLineError a
  }

instance Functor Mode where
  fmap f (Mode s p) = Mode s (fmap (fmap f) p)

-- | Makes a new 'Mode'.
mode
  :: String
  -- ^ Mode name
  -> [OptSpec a]
  -- ^ Mode options
  -> (String -> a)
  -- ^ Parses non-option positional arguments
  -> ([a] -> r)
  -- ^ Processes the result of all mode options
  -> Mode r
mode name os fPos fProc = Mode (ModeName name) prsr
  where
    prsr toks = case parseCommandLine os fPos strings of
      Left e -> Left e
      Right gs -> Right $ fProc gs
      where
        strings = map (\(Token x) -> x) toks

parseModeLine
  :: [OptSpec g]
  -- ^ Global options
  -> [Mode a]
  -- ^ All modes
  -> [String]
  -- ^ All command line tokens
  -> Either ModelineError ([g], Maybe a)
parseModeLine globals modes = pMode . parseGlobals globals . map Token
  where
    pMode (toks, glbls, mayGlblErr) = case toks of
      [] -> case mayGlblErr of
        Nothing -> Right (glbls, Nothing)
        Just (oes, ei) -> Left . GlobalOrLocal . Left . GlobalError oes $ lst
          where
            lst = case ei of
              Left oe -> LastGlobalOptsError oe
              Right opt -> LastWordError . Left $ opt

      modeTok : restToks -> case findExactMode modeTok modes of
        Left err -> Left (mergeLastGlobalError err mayGlblErr)
        Right mde -> case modeToParser mde restToks of
          Left cle ->
            Left (mergeCommandLineError (modeToModeName mde) cle mayGlblErr)
          Right r -> Right (glbls, Just r)

        
mergeCommandLineError
  :: ModeName
  -> CommandLineError
  -> Maybe ([OptionError], Either OptionError OptName)
  -> ModelineError
mergeCommandLineError name cle mayErr = case mayErr of
  Nothing -> GlobalOrLocal . Right
    . ModeCommandError name $ cle
  Just (oes, Left oe) -> GlobalAndLocal
    (GlobalError oes (LastGlobalOptsError oe))
    . ModeCommandError name $ cle
  Just (oes, Right opt) -> GlobalAndLocal
    (GlobalError oes (LastWordError . Left $ opt))
    . ModeCommandError name $ cle

-- | Adds an appropriate error for a bad mode.
addBadModeError
  :: Token
  -- ^ Token that purportedly was for the mode, but did not match any
  -- available mode
  -> Maybe ([OptionError], Either OptionError OptName)
  -- ^ Any errors that have arisen so far
  -> ModelineError
addBadModeError tok may = case may of
  Nothing -> GlobalOrLocal . Left . GlobalError [] . LastWordError
    . Right $ tok
  Just (oes, Left oe) -> GlobalOrLocal . Left . GlobalError (oes ++ [oe])
    . LastWordError . Right $ tok
  Just (oes, Right opt) -> GlobalOrLocal . Left . GlobalError oes
    . LastWordError . Left $ opt

{-
mergeLastGlobalError
  :: ModeError
  -> Maybe ([OptionError], Either OptionError OptName)
  -> ModelineError
mergeLastGlobalError me may = case may of
  Nothing -> GlobalOrLocal . Left . GlobalError [] . LastWordError
    . Right $ me
  Just (oes, Left oe) -> GlobalOrLocal . Left . GlobalError (oes ++ [oe])
    . LastWordError . Right $ me
  Just (oes, Right opt) -> GlobalOrLocal . Left . GlobalError oes
    . LastWordError . Left $ opt
-}

data GlobalResult
  = 

-- | Parses global options.
parseGlobals
  :: [OptSpec g]
  -- ^ Specification for global options.
  -> [Token]
  -- ^ All tokens present on the command line.
  -> ([Token], [g], Maybe ([OptionError], Either OptionError OptName))
  -- ^ Returns a triple @(a, b, c)@, where:
  --
  -- @a@ is all remaining tokens that were not parsed.  Because
  -- parsing stops with the first token that is NotAnOption, this
  -- list will include the mode token if there is one.
  --
  -- @b@ is the list of global options that were parsed.
  --
  -- @c@ indicates any errors.  If Nothing, there were no errors.  If
  -- @Just (d, e)@ then there were errors; @d@ indicates zero or more
  -- OptionError that may occur, while @e@ is the last error to occur.
  -- The last error is necessarily either a @Left OptionError@,
  -- indicating that the last error is a bad option or an option that
  -- erroneously has a long option, or a @Right OptName@, indicating
  -- that the last option did not have sufficient option arguments.
parseGlobals os toks =
  maddashOutToGlobalOut $ processTokens shrts lngs toks
  where
    (shrts, lngs) = splitOptSpecs os

-- | Takes a token and a list of all modes; returns the matching mode
-- if there is one, or Nothing if there is no match.
findExactMode
  :: Token
  -> [Mode a]
  -> Maybe (Mode a)
findExactMode _ [] = Nothing
findExactMode tok@(Token s) (m@(Mode (ModeName n) _) : ms)
  | s == n = Just m
  | otherwise = findExactMode tok ms

maddashOutToGlobalOut
  :: ([[Output a]], Either (OptName, Token -> ([Output a], State a)) [Token])
  -> ([Token], [a], Maybe ([OptionError], Either OptionError OptName))
maddashOutToGlobalOut (outs, ei) = (toksLeft, rslts, mayOptErrs)
  where
    toksLeft = case ei of
      Left _ -> []
      Right toks -> toks
    (rslts, errors) = partitionEithers . map f . concat $ outs
      where
        f (Good a) = Left a
        f (OptionError e) = Right e
    mayOptErrs = case (mayLast errors, ei) of
      (Nothing, Right _) -> Nothing
      (Nothing, Left (opt, _)) ->
        Just ([], Right opt)
      (Just (xs, x), Right _) ->
        Just (xs, Left x)
      (Just (xs, x), Left (opt, _)) ->
        Just (xs ++ [x], Right opt)

