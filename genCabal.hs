module Main where

import qualified Cartel as C

version :: C.Version
version = C.Version [0,30,0,0]

base :: C.Package
base = C.closedOpen "base" [4,5,0,0] [5]

bifunctors :: C.Package
bifunctors = C.Package "bifunctors" (Just $ C.gtEq [0,1,3,1])

containers :: C.Package
containers = C.Package "containers" (Just $ C.gtEq [0,4,2,1])

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

library
  :: [String]
  -- ^ List of all modules
  -> C.Library
library ms = C.Library
  [ C.buildDepends
    [ base
    , bifunctors
    , containers
    ]
  , C.hsSourceDirs [ "lib" ]
  , C.LibExposedModules ms
  , C.ghcOptions ["-Wall"]
  , C.defaultLanguage C.Haskell2010
  ]

cabal
  :: [String]
  -- ^ List of all modules
  -> C.Cabal
cabal ms = C.empty
  { C.cProperties = properties
  , C.cRepositories = [repo]
  , C.cLibrary = Just $ library ms
  }

main :: IO ()
main = do
  ms <- C.modules "lib"
  C.render "genCabal.hs" $ cabal ms
