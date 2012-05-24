-- | This is sample code using "System.Console.MultiArg". This could
-- be a command-line parser for the version of the Unix command @tail@
-- that is included with GNU coreutils version 8.5. "main" simply gets
-- the command line arguments, parses them, and prints out what was
-- parsed. To test it out, simply compile an executable that looks
-- like this and then feed it different options:
--
-- > import System.Console.MultiArg.SampleParser
-- > main = sampleMain
--
-- The code in the module is the sample code; the sample code is not
-- in the Haddock documentation! If you're reading this in Haddock,
-- you will want to also take a look at the actual source code.
module System.Console.MultiArg.SampleParser where

import System.Console.MultiArg.SimpleParser as P

data Flag =
  Bytes String
  | Follow (Maybe String)
  | Retry
  | Lines String
  | Stats String
  | Pid String
  | Quiet
  | Sleep String
  | Verbose
  | Help
  | Version
  | Filename String
  deriving Show

specs :: [P.OptSpec Flag]

specs =
  [ P.OptSpec ["bytes"]                     ['c']     (P.OneArg Bytes)
  , P.OptSpec ["follow"]                    ['f']     (P.OptionalArg Follow)
  , P.OptSpec ["follow-retry"]              ['F']     (P.NoArg Retry)
  , P.OptSpec ["lines"]                     ['n']     (P.OneArg Lines)
  , P.OptSpec ["max-unchanged-stats"]       []        (P.OneArg Stats)
  , P.OptSpec ["pid"]                       []        (P.OneArg Pid)
  , P.OptSpec ["quiet"]                     ['q']     (P.NoArg Quiet)
  , P.OptSpec ["sleep-interval"]            ['s']     (P.OneArg Sleep)
  , P.OptSpec ["verbose"]                   ['v']     (P.NoArg Verbose)
  , P.OptSpec ["help"]                      []        (P.NoArg Help)
  , P.OptSpec ["version"]                   []        (P.NoArg Version)
  ]

sampleMain :: P.Intersperse -> IO ()
sampleMain i = do
  as <- P.getArgs
  let r = P.parse i specs Filename as
  print r
