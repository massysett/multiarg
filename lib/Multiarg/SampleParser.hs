-- | This is sample code using "Multiarg". This could
-- be a command-line parser for the version of the Unix command @tail@
-- that is included with GNU coreutils version 8.5. "main" simply gets
-- the command line arguments, parses them, and prints out what was
-- parsed. To test it out, simply compile an executable that looks
-- like this and then feed it different options:
--
-- > import Multiarg.SampleParser
-- > main = sampleMain Intersperse
--
-- or:
--
-- > import Multiarg.SampleParser
-- > main = sampleMain StopOptions
--
-- The code in the module is the sample code; the sample code is not
-- in the Haddock documentation! If you're reading this in Haddock,
-- you will want to also take a look at the actual source code.
module Multiarg.SampleParser where

import qualified Multiarg.Combinator as C
import qualified Multiarg.CommandLine as P

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

specs :: [C.OptSpec Flag]

specs =
  [ C.OptSpec ["bytes"]                     ['c']
              (C.OneArg (return . Bytes))

  , C.OptSpec ["follow"]                    ['f']
              (C.OptionalArg (return . Follow))

  , C.OptSpec ["follow-retry"]              ['F']     (C.NoArg Retry)

  , C.OptSpec ["lines"]                     ['n']
              (C.OneArg (return . Lines))

  , C.OptSpec ["max-unchanged-stats"]       []
              (C.OneArg (return . Stats))

  , C.OptSpec ["pid"]                       []
              (C.OneArg (return . Pid))
  , C.OptSpec ["quiet"]                     ['q']     (C.NoArg Quiet)

  , C.OptSpec ["sleep-interval"]            ['s']
              (C.OneArg (return . Sleep))
  , C.OptSpec ["verbose"]                   ['v']     (C.NoArg Verbose)
  , C.OptSpec ["help"]                      []        (C.NoArg Help)
  , C.OptSpec ["version"]                   []        (C.NoArg Version)
  ]

sampleMain :: P.Intersperse -> IO ()
sampleMain i = do
  r <- P.simpleIO specs i (return . Filename)
  print r
