-- | Maddash is a Mealy finite state machine that processes options.
-- Ordinarily you will not need this module; instead, see "Multiarg"
-- for most uses or "Multiarg.Mode" for commands that have more than
-- one mode.
--
-- The machine consists of the following parts:
--
-- * The set of states, in 'State'
--
-- * The start state, which is 'Ready'
--
-- * The input alphabet, which is all 'Token's.  A 'Token' is an
-- input token from the command line.
--
-- * The output alphabet, which is 'Pallet'.  A 'Pallet' indicates
-- whether its input is not an option at all with 'NotAnOption'.  This
-- indicates that the input 'Token' was not a short option and was not
-- a long option; that is, it was not a single dash followed by a
-- non-dash character and it was not a double dash followed by another
-- character.  (Neither a single dash alone nor a double dash alone is
-- an option.)  Anything else is an option and will return 'Full',
-- which is a list of 'Output'.  Each 'Output' indicates either an
-- error or a good result.
--
-- * The transition function and the output function are combined into
-- a single function, 'processToken'.

module Multiarg.Maddash
  ( -- * Options and option arguments
    OptName(..)
  , ArgSpec(..)
  , ShortName
  , LongName
  , shortName
  , longName
  , shortNameToChar
  , longNameToString

  -- * Machine components
  , Token(..)
  , Output(..)
  , Pallet(..)
  , State(..)
  , isReady
  , isPending
  , processToken

  -- * Multi-token processor
  , processTokens

  -- * Errors
  , OptArg(..)
  , OptionError(..)
  ) where

import Control.Applicative

-- * Options and option arguments

-- | A short option name.
newtype ShortName = ShortName { shortNameToChar ::  Char }
  deriving (Eq, Ord, Show)

-- | A long option name.
newtype LongName = LongName { longNameToString :: String }
  deriving (Eq, Ord, Show)

-- | Creates a short option name.  Any character other than a single
-- hyphen will succeed.
shortName :: Char -> Maybe ShortName
shortName '-' = Nothing
shortName x = Just $ ShortName x

-- | Creates a long option name.  The string may not be empty, and the
-- first character may not be a hyphen.  In addition, no character may
-- be an equal sign.
longName :: String -> Maybe LongName
longName s = case s of
  [] -> Nothing
  '-':_ -> Nothing
  xs | '=' `elem` xs -> Nothing
     | otherwise -> Just $ LongName xs

-- | The name of an option (either short or long).
newtype OptName = OptName (Either ShortName LongName)
  deriving (Eq, Ord, Show)

data ArgSpec a
  = ZeroArg a
  -- ^ This option takes no arguments
  | OneArg (String -> a)
  -- ^ This option takes one argument
  | TwoArg (String -> String -> a)
  -- ^ This option takes two arguments
  | ThreeArg (String -> String -> String -> a)
  -- ^ This option takes three arguments

instance Functor ArgSpec where
  fmap f (ZeroArg a) = ZeroArg (f a)
  fmap f (OneArg g) = OneArg $ \a -> f (g a)
  fmap f (TwoArg g) = TwoArg $ \a b -> f (g a b)
  fmap f (ThreeArg g) = ThreeArg $ \a b c -> f (g a b c)

instance Show (ArgSpec a) where
  show (ZeroArg _) = "ZeroArg"
  show (OneArg _) = "OneArg"
  show (TwoArg _) = "TwoArg"
  show (ThreeArg _) = "ThreeArg"

-- * Machine components

-- | A token supplied by the user on the command line.
newtype Token = Token String
  deriving (Eq, Ord, Show)

data Output a
  = Good a
  | OptionError OptionError
  deriving (Eq, Ord, Show)

instance Functor Output where
  fmap f (Good a) = Good (f a)
  fmap _ (OptionError e) = OptionError e

data Pallet a
  = NotAnOption
  | Full [Output a]
  deriving (Eq, Ord, Show)

instance Functor Pallet where
  fmap _ NotAnOption = NotAnOption
  fmap f (Full os) = Full (map (fmap f) os)

data State a
  = Ready
  -- ^ Accepting new tokens

  | Pending OptName (Token -> ([Output a], State a))
  -- ^ In the middle of processing an option; this function will be
  -- applied to the next token to get a result


instance Functor State where
  fmap _ Ready = Ready
  fmap f (Pending o g)
    = Pending o (\t -> let (os, st') = g t
                       in (map (fmap f) os, fmap f st'))

instance Show (State a) where
  show Ready = "Ready"
  show (Pending o _) = "Pending - " ++ show o

isReady :: State a -> Bool
isReady Ready = True
isReady _ = False

isPending :: State a -> Bool
isPending (Pending _ _) = True
isPending _ = False

-- | Process a single token in the machine.
processToken
  :: [(ShortName, ArgSpec a)]
  -> [(LongName, ArgSpec a)]
  -> State a
  -> Token
  -> (Pallet a, State a)
processToken shorts longs st inp = case st of
  Pending _ f -> (Full os, st')
    where
      (os, st') = f inp
  Ready -> case procOpt of
    Just (os, st') -> (Full os, st')
    Nothing -> (NotAnOption, Ready)
    where
      procOpt = procShort shorts inp <|> procLong longs inp

-- * Multi-token processor

-- | Processes multiple tokens in the machine.  Processing ends with
-- the first token that is 'NotAnOption'.  This first token that is
-- 'NotAnOption', and all remaining tokens, are returned in the
-- result.  A list of all lists of 'Output' are also returned, with
-- one list for each input 'Token' that was processed.  Each of these
-- lists may be of any length.  For instance, if the input token is
-- the flag token for a long option that takes two arguments, the
-- corresponding list will be empty.  If the input token is a short
-- flag token, this list may have more than one element.
processTokens
  :: [(ShortName, ArgSpec a)]
  -> [(LongName, ArgSpec a)]
  -> [Token]
  -> ([[Output a]], Either (OptName, Token -> ([Output a], State a)) [Token])
processTokens shorts longs = go Ready
  where
    go Ready [] = ([], Right [])
    go (Pending opt f) [] = ([], Left (opt, f))
    go st (t:ts) = (rs, eiToksPend)
      where
        (pallet, st') = processToken shorts longs st t
        (rs, eiToksPend) = case pallet of
          NotAnOption -> ([], Right (t:ts))
          Full out -> (out : outRest, ei)
            where
              (outRest, ei) = go st' ts

-- * Errors

data OptionError
  = BadOption OptName
  | LongArgumentForZeroArgumentOption LongName OptArg
  -- ^ The uesr gave an argument for a long option that does not take
  -- an argument.
  deriving (Eq, Ord, Show)


-- * All exported types and functions above this line

-- * Other types - not exported

-- | An option argument.
newtype OptArg = OptArg { optArgToString :: String }
  deriving (Eq, Ord, Show)

-- | Characters after the first character in a short option; for
-- instance, if the user supplies @-afoobar@, then this will be
-- @foobar@.
newtype ShortTail = ShortTail String
  deriving (Eq, Ord, Show)

-- * Internal functions - not exported

tokenToOptArg :: Token -> OptArg
tokenToOptArg (Token t) = OptArg t


-- | Is this token an input for a long option?
isLong
  :: Token
  -> Maybe (LongName, Maybe OptArg)
  -- ^ Nothing if the option does not begin with a double dash and is
  -- not at least three characters long.  Otherwise, returns the
  -- characters following the double dash to the left of any equal
  -- sign.  The Maybe in the tuple is Nothing if there is no equal
  -- sign, or Just followed by characters following the equal sign if
  -- there is one.
isLong (Token ('-':'-':[])) = Nothing
isLong (Token ('-':'-':xs)) = Just (LongName optName, arg)
  where
    (optName, end) = span (/= '=') xs
    arg = case end of
      [] -> Nothing
      _:rs -> Just . OptArg $ rs
isLong _ = Nothing

-- | Is this the input token for a short argument?
isShort
  :: Token
  -> Maybe (ShortName, ShortTail)
isShort (Token ('-':'-':_)) = Nothing
isShort (Token ('-':[])) = Nothing
isShort (Token ('-':x:xs)) = Just (ShortName x, ShortTail xs)
isShort _ = Nothing

-- | Examines a token to determine if it is a short option.  If so,
-- processes it; otherwise, returns Nothing.
procShort
  :: [(ShortName, ArgSpec a)]
  -> Token
  -> Maybe ([Output a], State a)
procShort shorts inp = fmap (getShortOpt shorts) (isShort inp)

getShortOpt
  :: [(ShortName, ArgSpec a)]
  -> (ShortName, ShortTail)
  -> ([Output a], State a)
getShortOpt shorts (short, rest) = case lookup short shorts of
  Nothing -> ( [OptionError (BadOption (OptName (Left short))) ], Ready)
  Just arg -> procShortOpt shorts short arg rest

procShortOpt
  :: [(ShortName, ArgSpec a)]
  -> ShortName
  -> ArgSpec a
  -> ShortTail
  -> ([Output a], State a)
procShortOpt opts _ (ZeroArg a) (ShortTail inp) = (this : rest, st)
  where
    this = Good a
    (rest, st) = case inp of
      [] -> ([], Ready)
      opt : arg -> getShortOpt opts (ShortName opt, ShortTail arg)

procShortOpt _ shrt (OneArg f) (ShortTail inp) = case inp of
  [] -> ([], Pending opt g)
    where
      g tok = ([res], Ready)
        where
          res = Good . f . optArgToString $ arg
          arg = tokenToOptArg tok
  xs -> ([res], Ready)
    where
      res = Good . f . optArgToString $ optArg
      optArg = OptArg xs
  where
    opt = OptName (Left shrt)

procShortOpt _ shrt (TwoArg f) (ShortTail inp) = ([], Pending opt g)
  where
    g tok1 = case inp of
      [] -> ([], Pending opt h)
        where
          h tok2 = ([res], Ready)
            where
              oa2 = tokenToOptArg tok2
              res = Good $ f (optArgToString oa1) (optArgToString oa2)

      xs -> ([res], Ready)
        where
          res = Good $ f (optArgToString tokArg) (optArgToString oa1)
          tokArg = OptArg xs
      where
        oa1 = tokenToOptArg tok1
    opt = OptName (Left shrt)

procShortOpt _ shrt (ThreeArg f) (ShortTail inp) = ([], Pending opt g)
  where
    opt = OptName (Left shrt)
    g tok1 = ([], Pending opt h)
      where
        oa1 = tokenToOptArg tok1
        h tok2 = case inp of
          [] -> ([], Pending opt i)
            where
              i tok3 = ([res], Ready)
                where
                  oa3 = tokenToOptArg tok3
                  res = Good $ f (optArgToString oa1) (optArgToString oa2)
                                 (optArgToString oa3)
          tokInp -> ([res], Ready)
            where
              tokArg = tokenToOptArg (Token tokInp)
              res = Good $ f (optArgToString tokArg) (optArgToString oa1)
                             (optArgToString oa2)
          where
            oa2 = tokenToOptArg tok2

procLong
  :: [(LongName, ArgSpec a)]
  -> Token
  -> Maybe ([Output a], State a)
procLong longs inp = fmap (procLongOpt longs) (isLong inp)

procLongOpt
  :: [(LongName, ArgSpec a)]
  -> (LongName, Maybe OptArg)
  -> ([Output a], State a)
procLongOpt longs (inp, mayArg) = case lookup inp longs of
  Nothing -> ( [OptionError (BadOption . OptName . Right $ inp)], Ready)
  Just (ZeroArg r) -> ([result], Ready)
    where
      result = case mayArg of
        Nothing -> Good r
        Just arg -> OptionError (LongArgumentForZeroArgumentOption inp arg)

  Just (OneArg f) -> case mayArg of
    Nothing -> ([], Pending opt run)
      where
        run tok = ([out], Ready)
          where
            out = Good $ f (optArgToString arg1)
            arg1 = tokenToOptArg tok
    Just arg -> ([out], Ready)
      where
        out = Good $ f (optArgToString arg)

  Just (TwoArg f) -> ([], Pending opt g)
    where
      g gTok = case mayArg of
        Just arg1 -> ([out], Ready)
          where
            out = Good $ f (optArgToString arg1) (optArgToString gArg)
        Nothing -> ([], Pending opt h)
          where
            h hTok = ([out], Ready)
              where
                out = Good $ f (optArgToString gArg)
                               (optArgToString hArg)
                hArg = tokenToOptArg hTok
        where
          gArg = tokenToOptArg gTok

  Just (ThreeArg f) -> ([], Pending opt g)
    where
      g gTok = ([], Pending opt h)
        where
          gArg = tokenToOptArg gTok
          h hTok = case mayArg of
            Just arg1 -> ([out], Ready)
              where
                out = Good $ f (optArgToString arg1) (optArgToString gArg)
                               (optArgToString hArg)
            Nothing -> ([], Pending opt i)
              where
                i iTok = ([out], Ready)
                  where
                    iArg = tokenToOptArg iTok
                    out = Good $ f (optArgToString gArg)
                                   (optArgToString hArg)
                                   (optArgToString iArg)
            where
              hArg = tokenToOptArg hTok
  where
    opt = OptName (Right inp)

-- * end
