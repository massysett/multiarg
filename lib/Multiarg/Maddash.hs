-- | Maddash is a Mealy finite state machine that processes options.
-- The machine consists of the following parts:
--
-- * The set of states, in 'Multiarg.Maddash.State.T'
--
-- * The start state, which is 'Multiarg.Maddash.State.Empty'
--
-- * The input alphabet, which is all 'String's.  A 'String' is an
-- input token from the command line.
--
-- * The output alphabet, which is 'Multiarg.Maddash.Pallet.T'.  A
-- 'Multiarg.Maddash.Pallet.T' indicates whether its input is not an
-- option at all with 'Multiarg.Maddash.Pallet.NotAnOption'.  This
-- indicates that the input 'String' was not a short option and was
-- not a long option; that is, it was not a single dash followed by a
-- non-dash character and it was not a double dash followed by another
-- character.  (Neither a single dash alone nor a double dash alone is
-- an option.)  Anything else is an option and will return
-- 'Multiarg.Maddash.Pallet.Full', which is a list of
-- 'Multiarg.Maddash.Pallet.Output.T'.  Each
-- 'Multiarg.Maddash.Output.T' indicates either an error or a good
-- result.
--
-- * The transition function and the output function are combined into
-- a single function, 'Multiarg.Maddash.Transducer.transduce'.

module Multiarg.Maddash where

import Data.Map (Map)
import qualified Data.Map as M

newtype Option = Option (Either Short Long)
  deriving (Eq, Ord, Show)

data Error
  = BadShortOption Char
  | BadArgument1 Option OptArg Diagnostic
  | BadArgument2 Option OptArg OptArg Diagnostic
  | BadArgument3 Option OptArg OptArg OptArg Diagnostic
  | LongOptionNotFound Long
  | LongArgumentForZeroArgumentOption Long OptArg
  -- ^ The uesr gave an argument for a long option that does not take
  -- an argument.
  deriving (Eq, Ord, Show)

data Output a
  = Good a
  | Error Error
  deriving (Eq, Ord, Show)

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

-- | A diagnostic string returned if an argument is rejected.
newtype Diagnostic = Diagnostic String
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
  | OneArg (OptArg -> Either Diagnostic a)
  -- ^ This option takes one argument
  | TwoArg (OptArg -> OptArg -> Either Diagnostic a)
  -- ^ This option takes two arguments
  | ThreeArg (OptArg -> OptArg -> OptArg -> Either Diagnostic a)
  -- ^ This option takes three arguments


data State a
  = Ready
  -- ^ Accepting new tokens

  | Pending (Token -> ([Output a], State a))
  -- ^ In the middle of processing an option; this function will be
  -- applied to the next token to get a result

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

getShortOpt
  :: Map Short (ArgSpec a)
  -> (Short, ShortTail)
  -> ([Output a], State a)
getShortOpt = undefined

procShortOpt
  :: Map Short (ArgSpec a)
  -> ArgSpec a
  -> ShortTail
  -> ([Output a], State a)
procShortOpt opts (ZeroArg a) (ShortTail inp) = (this : rest, st)
  where
    this = Good a
    (rest, st) = case inp of
      [] -> ([], Ready)
      opt : arg -> getShortOpt opts (Short opt, ShortTail arg)

procShortOpt _ (OneArg f) (ShortTail inp) = case inp of
  [] -> ([], Pending g)
    where
      g tok = ([res], Ready)
        where
          res = case f arg of
            Left e -> Error (BadArgument1 opt 

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
  Nothing -> ( [Error (LongOptionNotFound inp)], Ready)
  Just (ZeroArg r) -> ([result], Ready)
    where
      result = case mayArg of
        Nothing -> Good r
        Just arg -> Error (LongArgumentForZeroArgumentOption inp arg)

  Just (OneArg f) -> case mayArg of
    Nothing -> ([], Pending run)
      where
        run tok = ([out], Ready)
          where
            out = case f arg1 of
              Left err -> Error (BadArgument1 opt arg1 err)
              Right good -> Good good
            arg1 = tokenToOptArg tok
    Just arg -> ([out], Ready)
      where
        out = case f arg of
          Left err -> Error (BadArgument1 opt arg err)
          Right g -> Good g

  Just (TwoArg f) -> ([], Pending g)
    where
      g gTok = case mayArg of
        Just arg1 -> ([out], Ready)
          where
            out = case f arg1 gArg of
              Left err -> Error (BadArgument2 opt arg1 gArg err)
              Right good -> Good good
        Nothing -> ([], Pending h)
          where
            h hTok = ([out], Ready)
              where
                out = case f gArg hArg of
                  Left err -> Error (BadArgument2 opt gArg hArg err)
                  Right good -> Good good
                hArg = tokenToOptArg hTok
        where
          gArg = tokenToOptArg gTok

  Just (ThreeArg f) -> ([], Pending g)
    where
      g gTok = ([], Pending h)
        where
          gArg = tokenToOptArg gTok
          h hTok = case mayArg of
            Just arg1 -> ([out], Ready)
              where
                out = case f arg1 gArg hArg of
                  Left err -> Error
                    (BadArgument3 opt arg1 gArg hArg err)
                  Right good -> Good good
            Nothing -> ([], Pending i)
              where
                i iTok = ([out], Ready)
                  where
                    iArg = tokenToOptArg iTok
                    out = case f gArg hArg iArg of
                      Left err -> Error
                        (BadArgument3 opt gArg hArg iArg err)
                      Right good -> Good good
            where
              hArg = tokenToOptArg hTok
  where
    opt = Option (Right inp)

