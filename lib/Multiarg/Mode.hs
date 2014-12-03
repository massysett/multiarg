-- | Helps you build command-line parsers for programs that have more
-- than one so-called /mode/; examples of such programs include @git@,
-- @darcs@, and @ghc-pkg@.
module Multiarg.Mode
  ( -- * Creating a single 'Mode'
    ArgSpec(..)
  , OptSpec(..)
  , Intersperse(..)
  , OptsWithPosArgs(..)
  , ModeName(..)
  , Mode(..)
  , mode

  -- * Parsing
  , parseModeLine

  -- * Errors
  , Short(..)
  , Long(..)
  , Option(..)
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
  = LastWordError (Either Option ModeError)
  | LastGlobalOptsError OptionError
  deriving (Eq, Ord, Show)

data ModeError
  = AmbiguousMode Token ModeName [ModeName]
  | ModeNotFound Token
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

data OptsWithPosArgs a = OptsWithPosArgs
  { opOpts :: [OptSpec a]
  , opIntersperse :: Intersperse
  , opPosArg :: String -> a
  }

instance Functor OptsWithPosArgs where
  fmap f (OptsWithPosArgs o i p) = OptsWithPosArgs (map (fmap f) o) i
    (fmap f p)

-- | Makes a new 'Mode'.
mode
  :: String
  -- ^ Mode name
  -> OptsWithPosArgs a
  -- ^ Mode options
  -> ([a] -> r)
  -- ^ Processes the result of all mode options
  -> Mode r
mode name (OptsWithPosArgs os int fPos) fProc = Mode (ModeName name) prsr
  where
    prsr toks = case parseCommandLine int os fPos strings of
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
parseModeLine globals modes = pMode . pGlobals
  where
    pMode (toks, glbls, mayGlblErr) = case toks of
      [] -> case mayGlblErr of
        Nothing -> Right (glbls, Nothing)
        Just (oes, ei) -> Left . GlobalOrLocal . Left . GlobalError oes $ lst
          where
            lst = case ei of
              Left oe -> LastGlobalOptsError oe
              Right opt -> LastWordError . Left $ opt

      modeTok : restToks -> case selectMode modeTok modes of
        Left err -> Left (mergeLastGlobalError err mayGlblErr)
        Right mde -> case modeToParser mde restToks of
          Left cle ->
            Left (mergeCommandLineError (modeToModeName mde) cle mayGlblErr)
          Right r -> Right (glbls, Just r)
        
    pGlobals = parseGlobals globals . map Token

mergeCommandLineError
  :: ModeName
  -> CommandLineError
  -> Maybe ([OptionError], Either OptionError Option)
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

mergeLastGlobalError
  :: ModeError
  -> Maybe ([OptionError], Either OptionError Option)
  -> ModelineError
mergeLastGlobalError me may = case may of
  Nothing -> GlobalOrLocal . Left . GlobalError [] . LastWordError
    . Right $ me
  Just (oes, Left oe) -> GlobalOrLocal . Left . GlobalError (oes ++ [oe])
    . LastWordError . Right $ me
  Just (oes, Right opt) -> GlobalOrLocal . Left . GlobalError oes
    . LastWordError . Left $ opt

parseGlobals
  :: [OptSpec g]
  -> [Token]
  -> ([Token], [g], Maybe ([OptionError], Either OptionError Option))
parseGlobals os toks =
  maddashOutToGlobalOut $ processTokens shrts lngs toks
  where
    (shrts, lngs) = splitOptSpecs os

findExactMode
  :: Token
  -> [Mode a]
  -> Maybe (Mode a)
findExactMode _ [] = Nothing
findExactMode tok@(Token s) (m@(Mode (ModeName n) _) : ms)
  | s == n = Just m
  | otherwise = findExactMode tok ms

selectMode
  :: Token
  -> [Mode a]
  -> Either ModeError (Mode a)
selectMode tok@(Token s) ms = case findExactMode tok ms of
  Just m -> Right m
  Nothing -> case filter f ms of
    [] -> Left (ModeNotFound tok)
    x:[] -> Right x
    x:xs -> Left (AmbiguousMode tok (modeToModeName x) (map modeToModeName xs))
    where
      f (Mode (ModeName n) _) = s `isPrefixOf` n

maddashOutToGlobalOut
  :: ([[Output a]], Either (Option, Token -> ([Output a], State a)) [Token])
  -> ([Token], [a], Maybe ([OptionError], Either OptionError Option))
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

