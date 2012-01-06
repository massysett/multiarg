-- | This is sample code using "System.Console.MultiArg". This could be
-- a command-line parser for the version of the Unix command @tail@
-- that is included with GNU coreutils version 8.5. "main" simply gets
-- the command line arguments, parses them, and prints out what was
-- parsed.
module System.Console.MultiArg.SampleParser where

import System.Console.MultiArg

specs :: [OptSpec String]

specs = [ OptSpec "bytes"        "c" ["bytes"]                 SOneArg
        , OptSpec "follow"       "f" ["follow"]                SOptionalArg
        , OptSpec "follow-retry" "F" []                        SNoArg
        , OptSpec "lines"        "n" ["lines"]                 SOneArg
        , OptSpec "states"       ""  ["max-unchanged-stats"]   SOneArg
        , OptSpec "pid"          ""  ["pid"]                   SOneArg
        , OptSpec "quiet"        "q" ["quiet", "silent"]       SNoArg
        , OptSpec "sleep"        "s" ["sleep-interval"]        SOneArg
        , OptSpec "verbose"      "v" ["verbose"]               SNoArg
        , OptSpec "help"         ""  ["help"]                  SNoArg
        , OptSpec "version"      ""  ["version"]               SNoArg
        ]

main :: IO ()
main = do
  as <- getArgs
  let r = parse Intersperse specs as
  print r
