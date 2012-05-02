-- | This is sample code using "System.Console.MultiArg". This could
-- be a command-line parser for the version of the Unix command @tail@
-- that is included with GNU coreutils version 8.5. "main" simply gets
-- the command line arguments, parses them, and prints out what was
-- parsed. To test it out, there is a @sample.hs@ file in the
-- @binaries@ directory of the multiarg archive that you can compile.
--
-- The code in the module is the sample code; the sample code is not
-- in the Haddock documentation! If you're reading this in Haddock,
-- you will want to also take a look at the actual source code.
module System.Console.MultiArg.SampleParser where

import System.Console.MultiArg.SimpleParser as P

-- | One way to use MultiArg is to write parsers that return each
-- different option using different data constructors in an algebraic
-- data type; we'll call this \"the algebraic method\".
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

specsAlgebraic :: [P.OptSpec Flag]
specsAlgebraic =
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

sampleAlgebraicMain :: IO ()
sampleAlgebraicMain = do
  as <- P.getArgs
  let r = P.parse P.Intersperse specsAlgebraic Filename as
  print r

-- | Another way to use MultiArg is to write parsers that return a
-- function that transforms a record; we'll call this \"the record
-- method\". This can be useful for writing more complex parsers. For
-- example, Flag type above returned all its arguments as
-- strings--even those which might be more useful as @Int@s. Here
-- let's have a record that contains all the options, with some of the
-- arguments converted to different types.
data Options = Options {
  tailSpec :: TailSpec
  , follow :: Maybe String
  , retry :: Bool
  , stats :: Maybe Int
  , pid :: Maybe Int
  , quiet :: Bool
  , sleep :: Maybe Int
  , verbose :: Bool
  , help :: Bool
  , version :: Bool
  , filenames :: [String]
  } deriving Show

data TailSpec = TBytes Int | TLines Int
              deriving Show

-- | Reads Int values without crashing if they are invalid.
readInt :: String -> P.Exceptional String Int
readInt s = case reads s of
  (x, ""):[] -> return x
  _ -> P.Exception $ "invalid number: " ++ s
