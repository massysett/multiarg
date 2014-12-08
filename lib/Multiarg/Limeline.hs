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
        
