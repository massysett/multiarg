module Main where

import Cartel

ver :: Version
ver = [0,30,0,10]

atLeast :: NonEmptyString -> [Word] -> Package
atLeast name ver = package name (gtEq ver)

base :: Package
base = closedOpen "base" [4,7,0,0] [5]

quickCheck :: Package
quickCheck = atLeast "QuickCheck" [2,7]

quickpull :: Package
quickpull = atLeast "quickpull" [0,4,0,0]

barecheck :: Package
barecheck = atLeast "barecheck" [0,2,0,6]

tasty :: Package
tasty = atLeast "tasty" [0,10]

tastyQuickcheck :: Package
tastyQuickcheck = atLeast "tasty-quickcheck" [0,8]

tastyTh :: Package
tastyTh = atLeast "tasty-th" [0,1]

properties :: Properties
properties = blank
  { name = "multiarg"
  , version = ver
  , cabalVersion = Just (1,18)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , copyright = "Copyright 2011-2015 Omari Norman"
  , author = "Omari Norman"
  , maintainer = "omari@smileystation.com"
  , stability = "Experimental"
  , homepage = "https://github.com/massysett/multiarg"
  , bugReports = "https://github.com/massysett/multiarg/issues"
  , synopsis = "Command lines for options that take multiple arguments"
  , description =
    [ "multiarg helps you build command-line parsers for"
    , "programs with options that take more than one argument."
    , "See the documentation in the Multiarg module for details."
    ]
  , category = "Console, Parsing"
  , extraSourceFiles =
    [ "ChangeLog", "README.md" ]
  }

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ hsSourceDirs [ "lib" ]
  , ghcOptions ["-Wall"]
  , haskell2010
  , buildDepends
    [ base
    ]
  ]

library
  :: [String]
  -- ^ List of all modules
  -> [LibraryField]
library ms = commonOptions ++
  [ exposedModules ms
  ]

tests
  :: [String]
  -- ^ Library modules
  -> [String]
  -- ^ Test modules
  -> Section
tests ms ts = testSuite "multiarg-tests" $ commonOptions ++
  [ exitcodeStdio
  , mainIs "multiarg-tests.hs"
  , otherModules (ms ++ ts)
  , hsSourceDirs [ "tests" ]
  , otherExtensions ["TemplateHaskell"]
  , testDepends
  ]

testDepends :: HasBuildInfo a => a
testDepends = buildDepends [ quickCheck, tasty, tastyQuickcheck, tastyTh ]

grover
  :: FlagName
  -- ^ Programs flag
  -> [String]
  -- ^ Library modules
  -> [String]
  -- ^ Test modules
  -> Section
grover fl ms ts = executable "grover"
  [ mainIs "grover-main.hs"
  , condBlock (flag fl)
    ( buildable True
    , commonOptions ++
        [ testDepends
        , otherModules (ms ++ ts)
        , hsSourceDirs ["tests"]
        ]
    )
    [ buildable False ]
  ]

telly
  :: FlagName
  -- ^ Programs flag
  -> [String]
  -- ^ Library modules
  -> [String]
  -- ^ Test modules
  -> Section
telly fl ms ts = executable "telly"
  [ mainIs "telly-main.hs"
  , condBlock (flag fl)
    ( buildable True
    , commonOptions ++
        [ testDepends
        , otherModules (ms ++ ts)
        , hsSourceDirs ["tests"]
        ]
    )
    [ buildable False ]
  ]

main :: IO ()
main = defaultMain $ do
  ms <- modules "lib"
  ts <- modules "tests"
  fl <- makeFlag "programs" $ FlagOpts
    { flagDescription = "Build sample programs"
    , flagDefault = False
    , flagManual = True
    }
  return
    ( properties
    , library ms
    , [ githubHead "massysett" "multiarg"
      , grover fl ms ts
      , telly fl ms ts
      , tests ms ts
      ]
    )
