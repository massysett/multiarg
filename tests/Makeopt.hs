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

processOption
  :: OptName
  -> Strings
  -- ^ Any option arguments
  -> StateK
  -> ([[String]], StateK)

processOption (OptName (Left shrtName)) ss ReadyK = case ss of
  NoStrings -> ([], PendingK shrt [])
  OneString s1 -> ([['-' : shrt : s1], [['-', shrt], s1]], ReadyK)
  TwoStrings s1 s2 ->
    ( [ ['-' : shrt : s1, s2 ]
      , ['-': shrt : [], s1, s2 ]
      ]
    , ReadyK
    )
  ThreeStrings s1 s2 s3 ->
    ( [ ['-' : shrt : s1, s2, s3]
      , ['-' : shrt : [], s1, s2, s3]
      ]
    , ReadyK
    )
  where
    shrt = shortNameToChar shrtName

processOption (OptName (Right lngName)) ss ReadyK = (strings, ReadyK)
  where
    strings = case ss of
      NoStrings -> [["--" ++ lng]]

      OneString s1 ->
        [ ["--" ++ lng ++ "=" ++ s1]
        , ["--" ++ lng, s1]
        ]

      TwoStrings s1 s2 ->
        [ ["--" ++ lng ++ "=" ++ s1]
        , ["--" ++ lng, s1, s2]
        ]

      ThreeStrings s1 s2 s3 ->
        [ ["--" ++ lng ++ "=" ++ s1]
        , ["--" ++ lng, s1, s2, s3]
        ]
    lng = longNameToString lngName

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
      NoStrings -> [[longOpt]]
      OneString s1 -> [[eqOpt s1], [longOpt, s1]]
      TwoStrings s1 s2 -> [[eqOpt s1, s2], [longOpt, s1, s2]]
      ThreeStrings s1 s2 s3 -> [[eqOpt s1, s2, s3], [longOpt, s1, s2, s3]]
    shorts = ejectShortFlags c1 cs
    longOpt = "--" ++ long
    eqOpt s = "--" ++ long ++ "=" ++ s
    long = longNameToString lngName

ejectShortFlags
  :: Char
  -- ^ First flag
  -> String
  -- ^ Remaining flags
  -> [[String]]
ejectShortFlags = undefined

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
shortPartitions = undefined
