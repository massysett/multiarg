-- | Processes both options and non-option positional arguments.
-- Functions here return both any successful results and any errors.
-- Ordinarily you will not need this module; instead, see "Multiarg"
-- for most uses or "Multiarg.Mode" for commands that have more than
-- one mode.
module Multiarg.Limeline where

import Multiarg.Maddash

data PosArg a = PosArg a
  deriving (Eq, Ord, Show)

instance Functor PosArg where
  fmap f (PosArg a) = PosArg (f a)

-- | Processes a command line where options are interspersed with
-- non-option positional arguments.  A stopper is not returned; all
-- tokens after a stopper are treated as non-option positional
-- arguments.
interspersed
  :: [(ShortName, ArgSpec a)]
  -> [(LongName, ArgSpec a)]
  -> (String -> a)
  -> [Token]
  -> ([Either [Output a] (PosArg a)], Maybe OptName)
interspersed shorts longs fTok = go
  where
    go toks = (map Left outs ++ outsRest, err)
      where
        (outs, ei) = processTokens shorts longs toks
        (outsRest, err) = case ei of
          Left (opt, _) -> ([], Just opt)
          Right [] -> ([], Nothing)
          Right ((Token x):xs)
            | x == "--" ->
                ( map (\(Token t) -> Right . PosArg . fTok $ t) xs
                , Nothing )
            | otherwise -> ( (Right . PosArg . fTok $ x) : rest
                           , mayErrRest )
            where
              (rest, mayErrRest) = go xs

