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
  let r = parse specs as
  print r
