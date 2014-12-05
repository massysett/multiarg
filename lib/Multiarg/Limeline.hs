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
    go toks = case ei of
      Left (opt, _) -> (map Left outs, Just opt)
      Right [] -> (map Left outs, Nothing)
      Right ((Token x):xs)
        | x == "--" -> (posArgsRest, Nothing)
        | otherwise -> ((Right . PosArg . fTok $ x) : outRest, mayRest)
        where
          (outRest, mayRest) = go xs
          posArgsRest = map (\(Token a) -> Right . PosArg . fTok $ a) xs
      where
        (outs, ei) = processTokens shorts longs toks
        
-- | Processes a command line where option processing terminates with
-- the first non-option positional argument.  A stopper is not
-- returned; all tokens after a stopper are treated as non-option
-- positional arguments.
nonInterspersed
  :: [(ShortName, ArgSpec a)]
  -> [(LongName, ArgSpec a)]
  -> (String -> a)
  -> [Token]
  -> ([Either [Output a] (PosArg a)], Maybe OptName)
nonInterspersed shorts longs fTok toks = (outs, mayOpt)
  where
    (outsOpts, ei) = processTokens shorts longs toks
    (outs, mayOpt) = case ei of
      Left (o, _) -> (map Left outsOpts, Just o)
      Right [] -> (map Left outsOpts, Nothing)
      Right (ls@(Token x : xs))
        | x == "--" -> (map Left outsOpts ++ map toPosArg xs, Nothing)
        | otherwise -> (map Left outsOpts ++ map toPosArg ls, Nothing)
        where
          toPosArg (Token tok) = Right . PosArg . fTok $ tok
