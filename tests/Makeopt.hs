-- | Makeopt is a Mealy finite state machine that produces all
-- possible partitions for a given set of command line options.

module Makeopt where

import Multiarg.Maddash

data StateK
  = ReadyK
  -- ^ Accepting new options
  | PendingK Char [Char]
  -- ^ In the middle of a short option
  deriving (Eq, Ord, Show)

data InputK
  = InputOpt OptName [String]
  | Eject
  deriving (Eq, Ord, Show)

processShortOptions
  :: [ShortName]
  -> (ShortName, [String])
  -> [[String]]
processShortOptions = undefined

processShortOption
  :: ShortName
  -> [String]
  -- ^ Any option arguments
  -> StateK
  -> ([[String]], StateK)

processShortOption shrtName ss ReadyK = case ss of
  [] -> ([], PendingK shrt [])
  x:xs -> (lists x xs, ReadyK)
  where
    shrt = shortNameToChar shrtName
    single = '-':shrt:[]
    combined s = '-':shrt:s
    lists a1 as
      | null a1 = [[single, a1] ++ as]
      | otherwise = [[combined a1] ++ as, [single, a1] ++ as]

processShortOption shrtName ss (PendingK c1 cs) = case ss of
  [] -> ([], PendingK c1 (cs ++ [shrt]))
  xs -> (shortPartitions c1 cs shrt xs, ReadyK)
  where
    shrt = shortNameToChar shrtName

processLongOption
  :: LongName
  -> [String]
  -> [[String]]
processLongOption lngName ss = lists ss
  where
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
