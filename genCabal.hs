module Main where

import qualified Cartel as C

version :: C.Version
version = C.Version [0,30,0,0]

base :: C.Package
base = C.closedOpen "base" [4,5,0,0] [5]

-- Note as of 2014-12-03
--
-- Do not use QuickCheck 2.7 features; this will
-- cause problems with Stackage, which currently builds using
-- QuickCheck 2.6 for older Haskell Platform versions and QuickCheck
-- 2.7 for GHC 7.8.  Should such features become necessary to use, you
-- will have to use shims to allow for compatibility with QuickCheck
-- versions 2.6 and 2.7.x.
quickCheck :: C.Package
quickCheck = C.closedOpen "QuickCheck" [2,6] [2,8]

quickpull :: C.Package
quickpull = C.closedOpen "quickpull" [0,4,0,0] [0,5]

barecheck :: C.Package
barecheck = C.closedOpen "barecheck" [0,2,0,6] [0,3]

properties :: C.Properties
properties = C.empty
  { C.prName = "multiarg"
  , C.prVersion = version
  , C.prCabalVersion = (1,20)
  , C.prBuildType = C.Simple
  , C.prLicense = C.BSD3
  , C.prLicenseFile = "LICENSE"
  , C.prCopyright = "Copyright 2011-2014 Omari Norman"
  , C.prAuthor = "Omari Norman"
  , C.prMaintainer = "omari@smileystation.com"
  , C.prStability = "Experimental"
  , C.prHomepage = "https://github.com/massysett/multiarg"
  , C.prBugReports = "https://github.com/massysett/multiarg/issues"
  , C.prSynopsis = "Command lines for options that take multiple arguments"
  , C.prDescription =
    [ "multiarg helps you build command-line parsers for"
    , "programs with options that take more than one argument."
    , "See the documentation in the Multiarg module for details."
    ]
  , C.prCategory = "Console, Parsing"
  , C.prExtraSourceFiles =
    [ "ChangeLog", "README.md" ]
  }

repo :: C.Repository
repo = C.empty
  { C.repoLocation = "git://github.com/massysett/multiarg.git"
  }

commonOptions :: C.Field a => [a]
commonOptions =
  [ C.hsSourceDirs [ "lib" ]
  , C.ghcOptions ["-Wall"]
  , C.defaultLanguage C.Haskell2010
  , C.buildDepends
    [ base
    ]
  ]

library
  :: [String]
  -- ^ List of all modules
  -> C.Library
library ms = C.Library $ commonOptions ++
  [ C.LibExposedModules ms
  ]

tests
  :: [String]
  -- ^ Test modules
  -> C.TestSuite
tests ms = C.TestSuite "multiarg-tests" $ commonOptions ++
  [ C.TestType C.ExitcodeStdio
  , C.TestMainIs "multiarg-tests.hs"
  , C.otherModules ms
  , C.hsSourceDirs [ "tests" ]
  , C.buildDepends
    [ quickCheck
    , quickpull
    , barecheck
    ]
  ]

cabal
  :: [String]
  -- ^ List of all modules
  -> [String]
  -- ^ Test modules
  -> C.Cabal
cabal ms ts = C.empty
  { C.cProperties = properties
  , C.cRepositories = [repo]
  , C.cLibrary = Just $ library ms
  , C.cTestSuites = [tests ts]
  }

main :: IO ()
main = do
  ms <- C.modules "lib"
  ts <- C.modules "tests"
  C.render "genCabal.hs" $ cabal ms ts
