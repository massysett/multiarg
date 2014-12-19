module Multiarg.Mode.Internal where

import Data.Either (partitionEithers)
import Multiarg.Maddash
import Multiarg.Internal
import Multiarg.Util
import Multiarg.Types

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

data Mode r = Mode ModeName ([Token] -> ParsedMode r)

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
  -> Either (String, [String]) (ModeResult g r)
getModeResult (GlobalLocal eis end)
  = combine global (endToModeResult end)
  where
    (glblErrs, glblGoods) = partitionEithers eis
    global = case glblErrs of
      [] -> Right glblGoods
      x:xs -> Left (x, xs)

combine
  :: Either (OptionError, [OptionError]) [g]
  -- ^ Global result.  Contains either one or more errors, or global
  -- option results.

  -> Either (String, [String]) (Either [String] r)
  -- ^ Result of parsing mode word, and the mode options and posargs.
  -- May be @Left a@, where @a@ is one or more errors, or @Right b@,
  -- where @b@ is a good result.  A good result @b@ may be either
  -- @Left c@, where @c@ is a list of non-option positional arguments,
  -- or @Right d@, where @d@ is the mode result.  @c@ indicates that
  -- no mode was recognized and may be either @[]@, which indicates
  -- that the user passed no words at all after the global options, or
  -- @x:xs@, indicating that the user did pass words after the global
  -- option, but the first word was not recognized as a mode.

  -> Either (String, [String]) (ModeResult g r)
combine (Left (oe1, oes)) (Left (me1, mes)) =
  Left ( optErrorToString oe1
       , map optErrorToString oes ++ (me1 : mes) )
combine (Left (oe1, oes)) (Right _) =
  Left (optErrorToString oe1, map optErrorToString oes)
combine (Right _) (Left (me1, mes)) = Left (me1, mes)
combine (Right glbls) (Right r) =
  Right (ModeResult glbls r)

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
extractParsedMode (ModeGood g) = Right . Right $ g
extractParsedMode (ModeError es lst) = Left $ case es of
  [] -> (eiToError lst, [])
  (x:xs) ->
    ( optErrorToString x
    , (map optErrorToString xs) ++ [eiToError lst] )

optErrorToString :: OptionError -> String
optErrorToString = undefined

eiToError :: Either OptionError OptName -> String
eiToError = undefined

optNameToError :: OptName -> String
optNameToError = undefined


parseModeLine
  :: [OptSpec g]
  -- ^ Global options
  -> [Mode r]
  -- ^ All modes
  -> [String]
  -- ^ All command line tokens
  -> (Either (String, [String]) (ModeResult g r))
parseModeLine glbl mds =
  getModeResult
  . parseModeLineWithErrors glbl mds

parseModeLineWithErrors
  :: [OptSpec g]
  -- ^ Global options
  -> [Mode r]
  -- ^ All modes
  -> [String]
  -- ^ All command line tokens
  -> GlobalLocal g r
parseModeLineWithErrors glbl mds tokStrings = GlobalLocal lsErrsGoods end
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
