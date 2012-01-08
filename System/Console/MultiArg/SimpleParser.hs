module System.Console.MultiArg.SimpleParser (
  ArgSpec(..),
  OptSpec(..),
  Intersperse(..),
  Result(..),
  SimpleError,
  getArgs,
  parse ) where

import System.Console.MultiArg.Prim
import System.Console.MultiArg.Combinator
import System.Console.MultiArg.Option
import Control.Monad.Exception.Synchronous ( toEither )
import System.Console.MultiArg.Error ( SimpleError )
import qualified System.Console.MultiArg.Error as E
import Data.Text ( pack, unpack )
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

data Intersperse = Intersperse | StopOptions

parse :: (Show o)
         => Intersperse
         -> [OptSpec o]
         -> [String]
         -> Either SimpleError [Result o]
parse i os ss = toEither $ runParser (map pack ss) (f os) where
  f = case i of Intersperse -> parseIntersperse
                StopOptions -> parseNoIntersperse

parseNoIntersperse :: Show o => [OptSpec o] -> Parser [Result o]
parseNoIntersperse os = do
  let opts = foldl1 (<|>) . map optSpec $ os
  rs <- manyTill (opts <?> expOptionOrPosArg) afterArgs
  firstArg <- afterArgs
  case firstArg of
    EndOfInput -> return rs
    (FirstArg s) -> do
      as <- noIntersperseArgs
      let first = PosArg s
      return $ rs ++ ( first : as )
    AAStopper -> do
      as <- noIntersperseArgs
      let first = Stopper
      return $ rs ++ ( first : as )

noIntersperseArgs :: Parser [Result o]
noIntersperseArgs = do
  as <- many nextArg
  let r = map PosArg . map unpack $ as
  return r

data AfterArgs = EndOfInput | FirstArg String | AAStopper

afterArgs :: Parser AfterArgs
afterArgs = parseFirst <|> parseEnd <|> parseStopper where
  parseFirst = do
    a <- nonOptionPosArg
    let aS = unpack a
    return $ FirstArg aS
  parseEnd = do
    end
    return EndOfInput
  parseStopper = do
    _ <- stopperParser
    return AAStopper

expOptionOrPosArg :: E.SimpleError -> E.SimpleError
expOptionOrPosArg (E.SimpleError _ s) =
  E.SimpleError E.ExpOptionOrPosArg s

parseIntersperse :: (Show o) => [OptSpec o] -> Parser [Result o]
parseIntersperse os = do
  let optsAndStopper = foldl1 (<|>) $ optSpecs ++ rest
      rest = [stopperParser, posArgParser]
      optSpecs = map optSpec os
  rs <- manyTill (optsAndStopper <?> expOptionOrPosArg) end
  end <?> error "the end parser should always succeed"
  return rs

stopperParser :: Parser (Result o)
stopperParser = stopper >> return Stopper

posArgParser :: Parser (Result o)
posArgParser = do
  a <- nonOptionPosArg
  return $ PosArg (unpack a)

optSpec :: (Show o) => OptSpec o -> Parser (Result o)
optSpec o = foldl1 (<|>) ls where
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
      (_, t) <- shortOneArg so
      return (OneArg l (unpack t))
    STwoArg -> do
      (_, t1, t2) <- shortTwoArg so
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
      (_, t) <- longOneArg lo
      return (OneArg l (unpack t))
    STwoArg -> do
      (_, t1, t2) <- longTwoArg lo
      return (TwoArg l (unpack t1) (unpack t2))
    SVariableArgs -> do
      (_, ts) <- longVariableArg lo
      return (VariableArgs l (map unpack ts))
    
