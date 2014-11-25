module Multiarg.Maddash.Transducer where

import qualified Multiarg.Maddash.State as State
import qualified Data.Map as M
import Data.Map (Map)
import qualified Multiarg.Maddash.Option.Long as Long
import qualified Multiarg.Maddash.Option.Short as Short
import qualified Multiarg.Maddash.Arguments as Arguments
import qualified Multiarg.Maddash.Output as Output
import qualified Multiarg.Maddash.Error as Error
import qualified Multiarg.Maddash.Pallet as Pallet
import Control.Applicative

transduce
  :: Map Short.T (Arguments.T a)
  -> Map Long.T (Arguments.T a)
  -> State.T a
  -> String
  -> (Pallet.T a, State.T a)
transduce shorts longs st inp = case st of
  State.Pending f -> (Pallet.Full os, st')
    where
      (os, st') = f inp
  State.Empty -> case procOpt of
    Just (os, st') -> (Pallet.Full os, st')
    Nothing -> (Pallet.NotAnOption, State.Empty)
    where
      procOpt = procShort shorts inp <|> procLong longs inp

-- | Is this token an input for a long option?
isLong
  :: String
  -- ^ Input token
  -> Maybe (String, Maybe String)
  -- ^ Nothing if the option does not begin with a double dash and is
  -- not at least three characters long.  Otherwise, returns the
  -- characters following the double dash to the left of any equal
  -- sign.  The Maybe in the tuple is Nothing if there is no equal
  -- sign, or Just followed by characters following the equal sign if
  -- there is one.
isLong ('-':'-':[]) = Nothing
isLong ('-':'-':xs) = Just (optName, arg)
  where
    (optName, end) = span (/= '=') xs
    arg = case end of
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
procLongOpt longs (inp, mayArg) = case M.lookup (Long.T inp) longs of
  Nothing -> ( [Output.Error (Error.LongOptionNotFound inp)]
             , State.Empty )
  Just (Arguments.Zero r) -> case mayArg of
    Nothing -> ([Output.Good r], State.Empty)
    Just arg ->
      ([Output.Error (Error.LongArgumentForZeroArgumentOption
          inp arg)], State.Empty)

  Just (Arguments.One f) -> case mayArg of
    Nothing -> ([], State.Pending run)
      where
        run str = case f str of
          Left err -> ( [Output.Error (Error.BadArgument1 str err)]
                      , State.Empty )
          Right g -> ( [Output.Good g], State.Empty)
    Just arg -> ([out], State.Empty)
      where
        out = case f arg of
          Left err -> Output.Error (Error.BadArgument1 arg err)
          Right g -> Output.Good g

  Just (Arguments.Two f) -> ([], State.Pending g)
    where
      g gArg = case mayArg of
        Just arg1 -> ([out], State.Empty)
          where
            out = case f arg1 gArg of
              Left err ->
                Output.Error (Error.BadArgument2 arg1 gArg err)
              Right good -> Output.Good good

        Nothing -> ([], State.Pending h)
          where
            h hArg = ([out], State.Empty)
              where
                out = case f gArg hArg of
                  Left err ->
                    Output.Error (Error.BadArgument2 gArg hArg err)
                  Right good -> Output.Good good

  Just (Arguments.Three f) -> ([], State.Pending g)
    where
      g gArg = ([], State.Pending h)
        where
          h hArg = case mayArg of
            Just arg1 -> ([out], State.Empty)
              where
                out = case f arg1 gArg hArg of
                  Left err ->
                    Output.Error (Error.BadArgument3 arg1 gArg hArg err)
                  Right good -> Output.Good good
            Nothing -> ([], State.Pending i)
              where
                i iArg = ([out], State.Empty)
                  where
                    out = case f gArg hArg iArg of
                      Left err -> Output.Error . Error.BadArgument3
                        gArg hArg iArg $ err
                      Right good -> Output.Good good
