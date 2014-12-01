module Multiarg.Limeline where

import Data.Map (Map)
import Multiarg.Maddash

data PosArg a = PosArg a
  deriving (Eq, Ord, Show)

instance Functor PosArg where
  fmap f (PosArg a) = PosArg (f a)

interspersed
  :: Map Short (ArgSpec a)
  -> Map Long (ArgSpec a)
  -> (String -> a)
  -> [Token]
  -> ([Either [Output a] (PosArg a)], Maybe Option)
interspersed shorts longs fTok = go
  where
    go toks = case ei of
      Left (opt, _) -> (map Left outs, Just opt)
      Right [] -> (map Left outs, Nothing)
      Right ((Token x):xs) -> ((Right . PosArg . fTok $ x) : outRest, eiRest)
        where
          (outRest, eiRest) = go xs
      where
        (outs, ei) = processTokens shorts longs toks
        
nonInterspersed
  :: Map Short (ArgSpec a)
  -> Map Long (ArgSpec a)
  -> (String -> a)
  -> [Token]
  -> ([Either [Output a] (PosArg a)], Maybe Option)
nonInterspersed shorts longs fTok toks = (outs, mayOpt)
  where
    (outsOpts, ei) = processTokens shorts longs toks
    (outs, mayOpt) = case ei of
      Left (o, _) -> (map Left outsOpts, Just o)
      Right tks -> (map Left outsOpts ++ outsPosArgs, Nothing)
        where
          outsPosArgs = map (\(Token x) -> Right . PosArg . fTok $ x) tks
