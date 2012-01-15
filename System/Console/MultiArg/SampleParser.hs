-- | This is sample code using "System.Console.MultiArg". This could be
-- a command-line parser for the version of the Unix command @tail@
-- that is included with GNU coreutils version 8.5. "main" simply gets
-- the command line arguments, parses them, and prints out what was
-- parsed.
module System.Console.MultiArg.SampleParser where

import System.Console.MultiArg

specs :: [OptSpec]

specs = [ OptSpec "bytes"               "c" []          oneArg
        , OptSpec "follow"              "f" []          optionalArg
        , OptSpec "follow-retry"        "F" []          noArg
        , OptSpec "lines"               "n" []          oneArg
        , OptSpec "max-unchanged-stats" ""  []          oneArg
        , OptSpec "pid"                 ""  []          oneArg
        , OptSpec "quiet"               "q" ["silent"]  noArg
        , OptSpec "sleep-interval"      "s" []          oneArg
        , OptSpec "verbose"             "v" []          noArg
        , OptSpec "help"                ""  []          noArg
        , OptSpec "version"             ""  []          noArg
        , OptSpec "multi-file"          "M" []          variableArg
        ]

sampleMain :: IO ()
sampleMain = do
  as <- getArgs
  let r = parse Intersperse specs as
  print r
