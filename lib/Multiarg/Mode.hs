-- | Helps you build command-line parsers for programs that have more
-- than one so-called /mode/; examples of such programs include @git@,
-- @darcs@, and @ghc-pkg@.
module Multiarg.Mode where

import Data.Either (partitionEithers)
import Multiarg.Maddash
import Multiarg
import Multiarg.Util

newtype ModeName = ModeName String
  deriving (Eq, Ord, Show)

data ParsedMode a
  = ModeGood a
  | ModeError [OptionError] (Either OptionError OptName)
  -- ^ There was an error.  There may be zero or more initial
  -- OptionError.  There must be at least one error, which is either
  -- an OptionError or the name of an option, if the error is that
  -- there were not enough words following the option to provide it
  -- with its necessary arguments.
  deriving (Eq, Ord, Show)

instance Functor ParsedMode where
  fmap f (ModeGood a) = ModeGood (f a)
  fmap _ (ModeError ls ei) = ModeError ls ei

data Mode a = Mode
  { modeToModeName :: ModeName
  , modeToParser :: [Token] -> ParsedMode a
  }

instance Functor Mode where
  fmap f (Mode s p) = Mode s (fmap (fmap f) p)

parsedCommandLineToParsedMode
  :: ([a] -> r)
  -> ParsedCommandLine a
  -> ParsedMode r
parsedCommandLineToParsedMode fMode (ParsedCommandLine ls mayOpt)
  = case mayOpt of
      Nothing -> case mayLast errs of
        Nothing -> ModeGood (fMode goods)
        Just (errs1st, errsLst) -> ModeError errs1st (Left errsLst)
      Just opt -> ModeError errs (Right opt)
  where
    (errs, goods) = partitionEithers ls

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
mode name opts fPos fMode
  = Mode (ModeName name)
  $ parsedCommandLineToParsedMode fMode
  . parseCommandLine opts fPos
  . map (\(Token s) -> s)

data GlobalLocalEnd a
  = GlobalInsufficientOptArgs OptName
  | ModeNotFound String [String]
  | NoMode
  | ModeFound (ParsedMode a)
  deriving (Eq, Ord, Show)

data GlobalLocal g r
  = GlobalLocal [Either OptionError g] (GlobalLocalEnd r)
  deriving (Eq, Ord, Show)

data ModeResult g r = ModeResult [g] (Either [String] r)
  deriving (Eq, Ord, Show)

getModeResult
  :: GlobalLocal g r
  -> Either [String] (ModeResult g r)
getModeResult (GlobalLocal eis end)
  = combine global (endToModeResult end)
  where
    (glblErrs, glblGoods) = partitionEithers eis
    global = case glblErrs of
      [] -> Right glblGoods
      x:xs -> Left (x, xs)
    combine = undefined

endToModeResult
  :: GlobalLocalEnd a
  -> Either (String, [String]) (Either [String] a)
endToModeResult end = case end of
  GlobalInsufficientOptArgs on -> Left (optNameToError on, [])
  ModeNotFound s ss -> Right (Left $ s:ss)
  NoMode -> Right (Left [])
  ModeFound pm -> extractParsedMode pm

extractParsedMode
  :: ParsedMode a
  -> Either (String, [String]) (Either b a)
extractParsedMode = undefined

optNameToError :: OptName -> String
optNameToError = undefined


parseModeLine
  :: [OptSpec g]
  -- ^ Global options
  -> [Mode r]
  -- ^ All modes
  -> [String]
  -- ^ All command line tokens
  -> GlobalLocal g r
parseModeLine glbl mds tokStrings = GlobalLocal lsErrsGoods end
  where
    toks = map Token tokStrings
    (shorts, longs) = splitOptSpecs glbl
    (outs, eiOptTok) = processTokens shorts longs toks
    lsErrsGoods = map f . concat $ outs
      where
        f (Good a) = Right a
        f (OptionError e) = Left e
    end = case eiOptTok of
      Left (opt, _) -> GlobalInsufficientOptArgs opt
      Right [] -> NoMode
      Right modeToks@(x:xs) -> case findExactMode x mds of
        Nothing -> ModeNotFound (unToken x) (map unToken xs)
          where
            unToken (Token t) = t
        Just (Mode _ f) -> ModeFound (f modeToks)

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

