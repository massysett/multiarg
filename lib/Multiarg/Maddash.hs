-- | Maddash is a Mealy finite state machine that processes options.
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

module Multiarg.Maddash where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative

newtype Option = Option (Either Short Long)
  deriving (Eq, Ord, Show)

data OptionError
  = BadOption Option
  | LongArgumentForZeroArgumentOption Long OptArg
  -- ^ The uesr gave an argument for a long option that does not take
  -- an argument.
  deriving (Eq, Ord, Show)

data Output a
  = Good a
  | OptionError OptionError
  deriving (Eq, Ord, Show)

instance Functor Output where
  fmap f (Good a) = Good (f a)
  fmap _ (OptionError e) = OptionError e

-- | A long option.  This should NOT be prefixed with the double dash.
newtype Long = Long String
  deriving (Eq, Ord, Show)

-- | A short option.
newtype Short = Short Char
  deriving (Eq, Ord, Show)

-- | An option argument.
newtype OptArg = OptArg String
  deriving (Eq, Ord, Show)

tokenToOptArg :: Token -> OptArg
tokenToOptArg (Token t) = OptArg t

-- | Characters after the first character in a short option; for
-- instance, if the user supplies @-afoobar@, then this will be
-- @foobar@.
newtype ShortTail = ShortTail String
  deriving (Eq, Ord, Show)

-- | A token supplied by the user on the command line.
newtype Token = Token String
  deriving (Eq, Ord, Show)

-- | Some of the functions return an @Either String a@.  In that case,
-- return a Left String to indicate an error, or a Right a to indicate
-- success.  The @String@ in the @Left String@ is an error message;
-- the faulty option is always indicated for you.  To supply
-- additional information in the rror, pass it in the @String@; if you
-- have no useful diagnostic information to add, just return an empty
-- @String@.
data ArgSpec a
  = ZeroArg a
  -- ^ This option takes no arguments
  | OneArg (OptArg -> a)
  -- ^ This option takes one argument
  | TwoArg (OptArg -> OptArg -> a)
  -- ^ This option takes two arguments
  | ThreeArg (OptArg -> OptArg -> OptArg -> a)
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

data State a
  = Ready
  -- ^ Accepting new tokens

  | Pending Option (Token -> ([Output a], State a))
  -- ^ In the middle of processing an option; this function will be
  -- applied to the next token to get a result

instance Functor State where
  fmap _ Ready = Ready
  fmap f (Pending o g)
    = Pending o (\t -> let (os, st') = g t
                       in (map (fmap f) os, fmap f st'))

data Pallet a
  = NotAnOption
  | Full [Output a]
  deriving (Eq, Ord, Show)

instance Functor Pallet where
  fmap _ NotAnOption = NotAnOption
  fmap f (Full os) = Full (map (fmap f) os)

-- | Process a single token in the machine.
processToken
  :: Map Short (ArgSpec a)
  -> Map Long (ArgSpec a)
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
  :: Map Short (ArgSpec a)
  -> Map Long (ArgSpec a)
  -> [Token]
  -> ([[Output a]], Either (Option, Token -> ([Output a], State a)) [Token])
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
    

-- | Is this token an input for a long option?
isLong
  :: Token
  -> Maybe (Long, Maybe OptArg)
  -- ^ Nothing if the option does not begin with a double dash and is
  -- not at least three characters long.  Otherwise, returns the
  -- characters following the double dash to the left of any equal
  -- sign.  The Maybe in the tuple is Nothing if there is no equal
  -- sign, or Just followed by characters following the equal sign if
  -- there is one.
isLong (Token ('-':'-':[])) = Nothing
isLong (Token ('-':'-':xs)) = Just (Long optName, arg)
  where
    (optName, end) = span (/= '=') xs
    arg = case end of
      [] -> Nothing
      _:rs -> Just . OptArg $ rs
isLong _ = Nothing

-- | Is this the input token for a short argument?
isShort
  :: Token
  -> Maybe (Short, ShortTail)
isShort (Token ('-':'-':_)) = Nothing
isShort (Token ('-':[])) = Nothing
isShort (Token ('-':x:xs)) = Just (Short x, ShortTail xs)
isShort _ = Nothing

-- | Examines a token to determine if it is a short option.  If so,
-- processes it; otherwise, returns Nothing.
procShort
  :: Map Short (ArgSpec a)
  -> Token
  -> Maybe ([Output a], State a)
procShort shorts inp = fmap (getShortOpt shorts) (isShort inp)

getShortOpt
  :: Map Short (ArgSpec a)
  -> (Short, ShortTail)
  -> ([Output a], State a)
getShortOpt shorts (short, rest) = case M.lookup short shorts of
  Nothing -> ( [OptionError (BadOption (Option (Left short))) ], Ready)
  Just arg -> procShortOpt shorts short arg rest

procShortOpt
  :: Map Short (ArgSpec a)
  -> Short
  -> ArgSpec a
  -> ShortTail
  -> ([Output a], State a)
procShortOpt opts _ (ZeroArg a) (ShortTail inp) = (this : rest, st)
  where
    this = Good a
    (rest, st) = case inp of
      [] -> ([], Ready)
      opt : arg -> getShortOpt opts (Short opt, ShortTail arg)

procShortOpt _ shrt (OneArg f) (ShortTail inp) = case inp of
  [] -> ([], Pending opt g)
    where
      g tok = ([res], Ready)
        where
          res = Good . f $ arg
          arg = tokenToOptArg tok
  xs -> ([res], Ready)
    where
      res = Good . f $ optArg
      optArg = OptArg xs
  where
    opt = Option (Left shrt)

procShortOpt _ shrt (TwoArg f) (ShortTail inp) = ([], Pending opt g)
  where
    g tok1 = case inp of
      [] -> ([], Pending opt h)
        where
          h tok2 = ([res], Ready)
            where
              oa2 = tokenToOptArg tok2
              res = Good $ f oa1 oa2

      xs -> ([res], Ready)
        where
          res = Good $ f tokArg oa1
          tokArg = OptArg xs
      where
        oa1 = tokenToOptArg tok1
    opt = Option (Left shrt)

procShortOpt _ shrt (ThreeArg f) (ShortTail inp) = ([], Pending opt g)
  where
    opt = Option (Left shrt)
    g tok1 = ([], Pending opt h)
      where
        oa1 = tokenToOptArg tok1
        h tok2 = case inp of
          [] -> ([], Pending opt i)
            where
              i tok3 = ([res], Ready)
                where
                  oa3 = tokenToOptArg tok3
                  res = Good $ f oa1 oa2 oa3
          tokInp -> ([res], Ready)
            where
              tokArg = tokenToOptArg (Token tokInp)
              res = Good $ f tokArg oa1 oa2
          where
            oa2 = tokenToOptArg tok2

procLong
  :: Map Long (ArgSpec a)
  -> Token
  -> Maybe ([Output a], State a)
procLong longs inp = fmap (procLongOpt longs) (isLong inp)

procLongOpt
  :: Map Long (ArgSpec a)
  -> (Long, Maybe OptArg)
  -> ([Output a], State a)
procLongOpt longs (inp, mayArg) = case M.lookup inp longs of
  Nothing -> ( [OptionError (BadOption . Option . Right $ inp)], Ready)
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
            out = Good $ f arg1
            arg1 = tokenToOptArg tok
    Just arg -> ([out], Ready)
      where
        out = Good $ f arg

  Just (TwoArg f) -> ([], Pending opt g)
    where
      g gTok = case mayArg of
        Just arg1 -> ([out], Ready)
          where
            out = Good $ f arg1 gArg
        Nothing -> ([], Pending opt h)
          where
            h hTok = ([out], Ready)
              where
                out = Good $ f gArg hArg
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
                out = Good $ f arg1 gArg hArg
            Nothing -> ([], Pending opt i)
              where
                i iTok = ([out], Ready)
                  where
                    iArg = tokenToOptArg iTok
                    out = Good $ f gArg hArg iArg
            where
              hArg = tokenToOptArg hTok
  where
    opt = Option (Right inp)

