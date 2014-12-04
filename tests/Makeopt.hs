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
  :: Option
  -> Strings
  -- ^ Any option arguments
  -> StateK
  -> ([[String]], StateK)

processOption (Option (Left (Short shrt))) ss ReadyK = case ss of
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

processOption (Option (Right (Long lng))) ss ReadyK = (strings, ReadyK)
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

