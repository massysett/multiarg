-- | Makeopt is a Mealy finite state machine that produces all
-- possible partitions for a given set of command line options.

module Makeopt where

import Multiarg.Maddash

data Strings
  = NoStrings
  | OneString String
  | TwoStrings String String
  | ThreeStrings String String String
  deriving (Eq, Ord, Show)

data StateK
  = ReadyK
  -- ^ Accepting new options
  | PendingK Char [Char]
  -- ^ In the middle of a short option
  deriving (Eq, Ord, Show)

processOption
  :: OptName
  -> Strings
  -- ^ Any option arguments
  -> StateK
  -> ([[String]], StateK)

processOption (OptName (Left shrtName)) ss ReadyK = case ss of
  NoStrings -> ([], PendingK shrt [])
  OneString s1 -> (lists s1 [], ReadyK)
  TwoStrings s1 s2 -> (lists s1 [s2], ReadyK)
  ThreeStrings s1 s2 s3 -> (lists s1 [s2, s3], ReadyK)
  where
    shrt = shortNameToChar shrtName
    single = '-':shrt:[]
    combined s = '-':shrt:s
    lists a1 as
      | null a1 = [[single, a1] ++ as]
      | otherwise = [[combined a1] ++ as, [single, a1] ++ as]

processOption (OptName (Right lngName)) ss ReadyK = (strings, ReadyK)
  where
    strings = case ss of
      NoStrings -> lists []
      OneString s1 -> lists [s1]
      TwoStrings s1 s2 -> lists [s1,s2]
      ThreeStrings s1 s2 s3 -> lists [s1,s2,s3]
    lng = "--" ++ longNameToString lngName
    lists [] = [[lng]]
    lists (x:xs) = [ (lng ++ "=" ++ x) : xs
                   , lng : x : xs
                   ]

processOption (OptName (Left shrtName)) ss (PendingK c1 cs) =
  case ss of
    NoStrings -> ([], PendingK c1 (cs ++ [shrt]))
    OneString s1 -> (shortPartitions c1 cs shrt [s1], ReadyK)
    TwoStrings s1 s2 -> (shortPartitions c1 cs shrt [s1, s2], ReadyK)
    ThreeStrings s1 s2 s3 ->
      (shortPartitions c1 cs shrt [s1, s2, s3], ReadyK)
  where
    shrt = shortNameToChar shrtName

processOption (OptName (Right lngName)) ss (PendingK c1 cs) =
  (shorts ++ res, ReadyK)
  where
    res = case ss of
      NoStrings -> lists []
      OneString s1 -> lists [s1]
      TwoStrings s1 s2 -> lists [s1,s2]
      ThreeStrings s1 s2 s3 -> lists [s1,s2,s3]
    shorts = ejectShortFlags c1 cs
    lng = "--" ++ longNameToString lngName
    lists [] = [[lng]]
    lists (x:xs) = [ (lng ++ "=" ++ x) : xs
                   , lng : x : xs
                   ]

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs]
  ++ [(x:ys):yss | (ys:yss) <- partitions xs]

ejectShortFlags
  :: Char
  -- ^ First flag
  -> String
  -- ^ Remaining flags
  -> [[String]]
ejectShortFlags c1 cs =
  map (map ('-':)) $ partitions (c1 : cs)

shortPartitions
  :: Char
  -- ^ First flag
  -> String
  -- ^ Remaining flags
  -> Char
  -- ^ Last flag
  -> [String] 
  -- ^ Arguments
  -> [[String]]
shortPartitions c1 cs cLast args = case args of
  [] -> flags
  x:xs -> together ++ separate
    where
      separate = [ list ++ (x:xs) | list <- flags ]
      together = do
        list <- flags
        case addToEnd list x of
          Nothing -> error "shortPartitions: error"
          Just r -> return $ r ++ xs
  where
    flags = ejectShortFlags c1 (cs ++ [cLast])
      
addToEnd :: [[a]] -> [a] -> Maybe [[a]]
addToEnd [] _ = Nothing
addToEnd xs toAdd = Just (init xs ++  [last xs ++ toAdd])
