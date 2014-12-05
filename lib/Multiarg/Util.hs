module Multiarg.Util where

import Multiarg.Types
import Multiarg.Maddash

mayLast :: [a] -> Maybe ([a], a)
mayLast [] = Nothing
mayLast xs = Just (init xs, last xs)

parsedOpts :: [Either () a] -> Maybe [a]
parsedOpts [] = Just []
parsedOpts (Left _ : _) = Nothing
parsedOpts (Right x : xs) = case parsedOpts xs of
  Nothing -> Nothing
  Just rest -> Just (x : rest)

splitOptSpecs
  :: [OptSpec a]
  -> ([(ShortName, ArgSpec a)], [(LongName, ArgSpec a)])
splitOptSpecs = foldr f ([], [])
  where
    f (OptSpec so lo sp) (ss, ls) = (so' ++ ss, lo' ++ ls)
      where
        so' = map (\o -> (o, sp)) so
        lo' = map (\o -> (o, sp)) lo

addHelpOption
  :: [OptSpec a]
  -> ( [(ShortName, ArgSpec (Either () a))]
     , [(LongName, ArgSpec (Either () a))] )
addHelpOption os = splitOptSpecs os'
  where
    os' = optSpec "h" ["help"] (ZeroArg (Left ())) : map (fmap Right) os

