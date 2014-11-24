module Multiarg.Machine.Transducer where

import qualified Multiarg.Machine.State as State
import qualified Data.Map as M
import Data.Map (Map)
import qualified Multiarg.Machine.Option.Long as Long
import qualified Multiarg.Machine.Option.Short as Short
import qualified Multiarg.Machine.Arguments as Arguments
import qualified Multiarg.Machine.Output as Output
import qualified Multiarg.Machine.Error as Error
import Control.Applicative

transduce
  :: Map Short.T (Arguments.T a)
  -> Map Long.T (Arguments.T a)
  -> (String -> Either String a)
  -> State.T a
  -> String
  -> ([Output.T a], State.T a)
transduce shorts longs fArg st inp = case st of
  State.Pending f -> f inp
  State.Empty -> case procOpt of
    Just r -> r
    Nothing -> procArg fArg inp
    where
      procOpt = procShort shorts inp <|> procLong longs inp

-- | Is this token an input for a long option?
isLong
  :: String
  -- ^ Input token
  -> Maybe (String, Maybe String)
  -- ^ Nothing if the option does not begin with a double dash
  -- followed by at least one non-equal sign character.  Otherwise,
  -- returns the characters following the double dash to the left of
  -- any equal sign.  The Maybe in the tuple is Nothing if there is no
  -- equal sign, or Just followed by characters following the equal
  -- sign if there is one.
isLong ('-':'-':xs) = case span (/= '=') xs of
  ([], _) -> Nothing
  (name, rest) -> Just (name, arg)
    where
      arg = case rest of
        [] -> Nothing
        _:rs -> Just rs
isLong _ = Nothing

-- | Is this the input token for a short argument?
isShort
  :: String
  -> Maybe (Char, String)
isShort ('-':'-':_) = Nothing
isShort ('-':[]) = Nothing
isShort ('-':x:xs) = Just (x, xs)
isShort _ = Nothing

-- | Examines a token to determine if it is a short option.  If so,
-- processes it; otherwise, returns Nothing.
procShort
  :: Map Short.T (Arguments.T a)
  -> String
  -> Maybe ([Output.T a], State.T a)
procShort shorts inp = fmap (procShortOpt shorts) (isShort inp)

procShortOpt
  :: Map Short.T (Arguments.T a)
  -> (Char, String)
  -> ([Output.T a], State.T a)
procShortOpt shorts (arg, rest) =
  case M.lookup (Short.T arg) shorts of
    Nothing -> ( [Output.Error (Error.BadShortOption arg)]
               , State.Empty )
    Just ag -> procShortArg shorts ag rest

procShortArg
  :: Map Short.T (Arguments.T a)
  -> Arguments.T a
  -> String
  -- ^ The argument to process
  -> ([Output.T a], State.T a)
procShortArg opts (Arguments.Zero r) inp = (this : rest, st)
  where
    this = Output.Good r
    (rest, st) = case inp of
      [] -> ([], State.Empty)
      opt:arg -> procShortOpt opts (opt, arg)

procShortArg _ (Arguments.One f) inp = ([this], State.Empty)
  where
    this = case f inp of
      Left e -> Output.Error (Error.BadArgument1 inp e)
      Right g -> Output.Good g

procShortArg _ (Arguments.Two f) inp = ([], State.Pending f')
  where
    f' inp2 = ([this], State.Empty)
      where
        this = case f inp inp2 of
          Left e -> Output.Error (Error.BadArgument2 inp inp2 e)
          Right g -> Output.Good g

procShortArg _ (Arguments.Three f) inp = ([], State.Pending f')
  where
    f' inp2 = ([], State.Pending f'')
      where
        f'' inp3 = ([this], State.Empty)
          where
            this = case f inp inp2 inp3 of
              Left e ->
                Output.Error (Error.BadArgument3 inp inp2 inp3 e)
              Right g -> Output.Good g
      

procLong
  :: Map Long.T (Arguments.T a)
  -> String
  -> Maybe ([Output.T a], State.T a)
procLong longs inp = fmap (procLongOpt longs) (isLong inp)

procLongOpt
  :: Map Long.T (Arguments.T a)
  -> (String, Maybe String)
  -> ([Output.T a], State.T a)
procLongOpt = undefined

selectLongOpt
  :: Map Long.T (Arguments.T a)
  -> String
  -> Either Error.T (Arguments.T a)
selectLongOpt = undefined

procArg
  :: (String -> Either String a)
  -> String
  -> ([Output.T a], State.T a)
procArg = undefined
