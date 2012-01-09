-- | This is sample code using "System.Console.MultiArg". This could be
-- a command-line parser for the version of the Unix command @tail@
-- that is included with GNU coreutils version 8.5. "main" simply gets
-- the command line arguments, parses them, and prints out what was
-- parsed.
module System.Console.MultiArg.SampleParser where

import System.Console.MultiArg

specs :: [OptSpec]

specs = [ OptSpec "bytes"               "c" []          SOneArg
        , OptSpec "follow"              "f" []          SOptionalArg
        , OptSpec "follow-retry"        "F" []          SNoArg
        , OptSpec "lines"               "n" []          SOneArg
        , OptSpec "max-unchanged-stats" ""  []          SOneArg
        , OptSpec "pid"                 ""  []          SOneArg
        , OptSpec "quiet"               "q" ["silent"]  SNoArg
        , OptSpec "sleep-interval"      "s" []          SOneArg
        , OptSpec "verbose"             "v" []          SNoArg
        , OptSpec "help"                ""  []          SNoArg
        , OptSpec "version"             ""  []          SNoArg
        ]

sampleMain :: IO ()
sampleMain = do
  as <- getArgs
  let r = parse Intersperse specs as
  print r
