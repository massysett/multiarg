module Multiarg.Limeline where

import Data.Map (Map)
import Multiarg.Maddash

data PosArg a
  = PosArgError Diagnostic
  | GoodPosArg a
  deriving (Eq, Ord, Show)

processPosArg :: (String -> Either String a) -> Token -> PosArg a
processPosArg f (Token t) = case f t of
  Left e -> PosArgError (Diagnostic e)
  Right g -> GoodPosArg g

instance Functor PosArg where
  fmap _ (PosArgError d) = PosArgError d
  fmap f (GoodPosArg a) = GoodPosArg (f a)

interspersed
  :: Map Short (ArgSpec a)
  -> Map Long (ArgSpec a)
  -> (String -> Either String a)
  -> [Token]
  -> ([Either [Output a] (PosArg a)], Maybe Option)
interspersed shorts longs fTok = go
  where
    go toks = case ei of
      Left (opt, _) -> (map Left outs, Just opt)
      Right [] -> (map Left outs, Nothing)
      Right (x:xs) -> (Right (processPosArg fTok x) : outRest, eiRest)
        where
          (outRest, eiRest) = go xs
      where
        (outs, ei) = processTokens shorts longs toks
        
nonInterspersed
  :: Map Short (ArgSpec a)
  -> Map Long (ArgSpec a)
  -> (String -> Either String a)
  -> [Token]
  -> ([Either [Output a] (PosArg a)], Maybe Option)
nonInterspersed shorts longs fTok toks = (outs, mayOpt)
  where
    (outsOpts, ei) = processTokens shorts longs toks
    (outs, mayOpt) = case ei of
      Left (o, _) -> (map Left outsOpts, Just o)
      Right tks -> (map Left outsOpts ++ outsPosArgs, Nothing)
        where
          outsPosArgs = map (\o -> Right $ processPosArg fTok o) tks
