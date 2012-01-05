module System.Console.MultiArg.SimpleParser (
  ArgSpec(..),
  OptSpec(..),
  Result(..),
  SimpleError,
  getArgs,
  parse ) where

import System.Console.MultiArg.Prim
import System.Console.MultiArg.Combinator
import System.Console.MultiArg.Option
import System.Console.MultiArg.Prim ( runParser )
import Control.Monad.Exception.Synchronous ( toEither )
import System.Console.MultiArg.Error ( SimpleError )
import qualified System.Console.MultiArg.Error as E
import Data.Text ( Text, pack, unpack )
import System.Environment ( getArgs )

data ArgSpec = SNoArg | SOptionalArg | SOneArg | STwoArg | SVariableArgs
             deriving Show

data OptSpec o = OptSpec { label :: o
                         , shortOpts :: [Char]
                         , longOpts :: [String]
                         , argSpec :: ArgSpec }
               deriving Show

data Result o =
  NoArg { fLabel :: o }

  | OptionalArg { oLabel :: o
                , oArg :: Maybe String }

  | OneArg   { sLabel :: o
             , sArg1 :: String }

  | TwoArg   { dLabel :: o
             , dArg1 :: String
             , dArg2 :: String }

  | VariableArgs { vLabel :: o
                 , vArgs :: [String] }

  | PosArg { posArg :: String }
    
  | Stopper
  deriving Show

parse :: (Show o)
         => Bool -- ^ Allow intersperse?
         -> [OptSpec o]
         -> [String]
         -> Either SimpleError [Result o]
parse i os ss = toEither $ runParser (map pack ss) (f os) where
  f = if i then parseIntersperse else parseNoIntersperse

parseNoIntersperse :: Show o => [OptSpec o] -> Parser [Result o]
parseNoIntersperse = undefined
{-
parseNoIntersperse os = do
  let e = E.unexpected E.ExpOptionOrPosArg E.SawNoOptionOrPosArg
      opts = choice e (map optSpec os)
  rs <- many 
-}

parseIntersperse :: (Show o) => [OptSpec o] -> Parser [Result o]
parseIntersperse os = do
  let e = E.unexpected E.ExpOptionOrPosArg E.SawNoOptionOrPosArg
      optsAndStopper = choice e (optSpecs ++ rest)
      rest = [stopperParser, posArgParser]
      optSpecs = map optSpec os
  rs <- many (require optsAndStopper)
  end
  return rs

stopperParser :: Parser (Result o)
stopperParser = stopper >> return Stopper

posArgParser :: Parser (Result o)
posArgParser = do
  a <- nonOptionPosArg
  return $ PosArg (unpack a)

optSpec :: (Show o) => OptSpec o -> Parser (Result o)
optSpec o = choice e ls where
  e = E.unexpected (E.ExpOption (pack . show $ o)) E.SawNoOption
  ls = shorts ++ longs
  shorts = map (shortOpt (label o) (argSpec o)) (shortOpts o)
  longs = map (longOpt (label o) (argSpec o)) (longOpts o)

shortOpt :: o -> ArgSpec -> Char -> Parser (Result o)
shortOpt l s c = let
  so = makeShortOpt c in case s of
    SNoArg -> do
      _ <- shortNoArg so
      return (NoArg l)
    SOptionalArg -> do
      (_, mt) <- shortOptionalArg so
      return (OptionalArg l (fmap unpack mt))
    SOneArg -> do
      (_, t) <- shortSingleArg so
      return (OneArg l (unpack t))
    STwoArg -> do
      (_, t1, t2) <- shortDoubleArg so
      return (TwoArg l (unpack t1) (unpack t2))
    SVariableArgs -> do
      (_, ts) <- shortVariableArg so
      return (VariableArgs l (map unpack ts))
    
longOpt :: o -> ArgSpec -> String -> Parser (Result o)
longOpt l s c = let
  lo = makeLongOpt (pack c) in case s of
    SNoArg -> do
      _ <- longNoArg lo
      return (NoArg l)
    SOptionalArg -> do
      (_, mt) <- longOptionalArg lo
      return (OptionalArg l (fmap unpack mt))
    SOneArg -> do
      (_, t) <- longSingleArg lo
      return (OneArg l (unpack t))
    STwoArg -> do
      (_, t1, t2) <- longDoubleArg lo
      return (TwoArg l (unpack t1) (unpack t2))
    SVariableArgs -> do
      (_, ts) <- longVariableArg lo
      return (VariableArgs l (map unpack ts))
    
