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
  -> ([(Short, ArgSpec a)], [(Long, ArgSpec a)])
splitOptSpecs = foldr f ([], [])
  where
    f (OptSpec so lo sp) (ss, ls) = (shrts ++ ss, lngs ++ ls)
      where
        shrts = map (\c -> (Short c, sp)) so
        lngs = map (\l -> (Long l, sp)) lo

addHelpOption
  :: [OptSpec a]
  -> ([(Short, ArgSpec (Either () a))], [(Long, ArgSpec (Either () a))])
addHelpOption os = splitOptSpecs os'
  where
    os' = OptSpec "h" ["help"] (ZeroArg (Left ())) : map (fmap Right) os

