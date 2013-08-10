module Main where

import Control.Monad (liftM2, liftM4)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Ord (comparing)
import Data.List (group, sort)
import qualified System.Console.MultiArg as A
import qualified Test.QuickCheck as Q
import Test.QuickCheck (Arbitrary, arbitrary, Gen, suchThat)
import Data.Functor.Identity (Identity(..))

instance Q.Arbitrary A.ShortOpt where
  arbitrary = do
    c <- Q.arbitrary `Q.suchThat` (/= '-')
    case A.makeShortOpt c of
      Nothing -> error "failed to generate short opt"
      Just r -> return r

instance Q.Arbitrary A.LongOpt where
  arbitrary = do
    s <- Q.listOf1 (Q.arbitrary `Q.suchThat` (/= '='))
    case A.makeLongOpt s of
      Nothing -> error "failed to generate long opt"
      Just r -> return r

data GArgSpec
  = GNoArg
  | GOptionalArg
  | GOneArg
  | GTwoArg
  | GVariableArg
  | GChoiceArg [(String, Int)]
  deriving (Eq, Show)

instance Arbitrary GArgSpec where
  arbitrary = Q.oneof $
    (map return [ GNoArg, GOptionalArg, GOneArg, GTwoArg,
                  GVariableArg ]) ++ [(fmap GChoiceArg genChoiceArgs)]
        
genChoiceArgs :: Gen [(String, Int)]
genChoiceArgs = do
  let genStr = Q.listOf1 arbitrary
  ss <- listOf1Unique genStr
  return $ zip ss [0..]

data GOption = GOption
  { gId :: Int
  , gLongs :: [A.LongOpt]
  , gShorts :: [A.ShortOpt]
  , gOptSpec :: GArgSpec
  } deriving (Eq, Show)

instance Arbitrary GOption where
  arbitrary = liftM4 GOption arbitrary arbitrary arbitrary arbitrary
              `suchThat` p
    where
      p (GOption _ ls ss _) = (length ls + length ss) > 0

gOptionList :: Gen [GOption]
gOptionList = arbitrary `suchThat` nodupe
  where
    nodupe ls = (noDupes . map gId $ ls)
                && (noDupes . concatMap gLongs $ ls)
                && (noDupes . concatMap gShorts $ ls)

noDupes :: (Eq a, Ord a) => [a] -> Bool
noDupes = all ((== 1) . length) . group . sort

listOfUnique :: (Eq a, Ord a) => Gen a -> Gen [a]
listOfUnique g = Q.listOf g `Q.suchThat` noDupes

listOf1Unique :: (Eq a, Ord a) => Gen a -> Gen [a]
listOf1Unique g = Q.listOf1 g `Q.suchThat` noDupes


data GIntr = GIntersperse | GStopOpts deriving (Eq, Show, Ord)

instance Arbitrary GIntr where
  arbitrary = Q.elements [GIntersperse, GStopOpts]

data GOpts = GOpts
  { gOpts :: [GOption]
  , gShortcuts :: [GOption]
  } deriving (Eq, Show)

instance Arbitrary GOpts where
  arbitrary = liftM2 GOpts gOptionList gOptionList

data GOptsWithPosArgs = GOptsWithPosArgs
  { goOpts :: GOpts
  , goIntr :: GIntr
  } deriving (Eq, Show)

instance Arbitrary GOptsWithPosArgs where
  arbitrary = liftM2 GOptsWithPosArgs arbitrary arbitrary

--
--
--

data ROption = ROption
  { rId :: Int
  , rOpt :: Either A.LongOpt A.ShortOpt
  , rArgSpec :: RArgSpec
  } deriving (Eq, Show)

data RPreStopperPosArg = RPreStopperPosArg
  { preArg :: String }
  deriving (Eq, Show, Ord)

data RPostStopperPosArg = RPostStopperPosArg
  { postArg :: String }
  deriving (Eq, Show, Ord)

--
--
--

data RArgSpec
  = RNoArg
  | ROptionalArg (Maybe String)
  | ROneArg String
  | RTwoArg (String, String)
  | RThreeArg (String, String, String)
  | RVariableArg [String]
  | RChoiceArg Int
  deriving (Eq, Show)

data Opt = Opt
  { optId :: Int
  , optArg :: RArgSpec
  } deriving (Eq, Show)

data SimplePure = SimplePure
  { spOpts :: A.OptsWithPosArgs Opt Opt
  , spArgs :: [String]
  , spResult :: Ex.Exceptional A.Error (Either Opt [Opt])
  }

instance Show SimplePure where
  show = undefined

prop_simple :: SimplePure -> Bool
prop_simple (SimplePure os ss r) = A.simplePure os ss == r

main :: IO ()
main = undefined
