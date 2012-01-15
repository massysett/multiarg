-- | A simple command line parser that can parse options that take an
-- optional argument, one or two arguments, or a variable number of
-- arguments. For sample code that uses this parser, see
-- 'System.Console.MultiArg.SampleParser'.
module System.Console.MultiArg.SimpleParser (
  ArgSpec(..),
  OptSpec(..),
  Intersperse(..),
  Result(..),
  Args(..),
  SimpleError,
  getArgs,
  System.Console.MultiArg.SimpleParser.parse ) where

import System.Console.MultiArg.Prim
import qualified System.Console.MultiArg.Prim as Prim
import System.Console.MultiArg.Combinator
import System.Console.MultiArg.Option
import Control.Monad.Exception.Synchronous ( toEither )
import System.Console.MultiArg.Error ( SimpleError )
import Data.Text ( pack, unpack )
import System.Environment ( getArgs )
import Data.Monoid ( mconcat )
import Control.Applicative ( many, (<|>) )

-- | Specifies each option that your program accepts.
data OptSpec = OptSpec {
  longOpt :: String
  -- ^ Each option must have at least one long option, which you
  -- specify here. Your program's users specify long options by
  -- preceding them with two dashes, such as @--verbose@. When writing
  -- your code you omit the dashes, so you would specify @verbose@
  -- here; including the dashes in your code results in a runtime
  -- error.

  , shortOpts :: [Char]
    -- ^ Additional, synonymous short options may be specified
    -- here. For instance if you want your users to be able to specify
    -- @-v@ in addition to @--verbose@, include @v@ in this list.
    
  , longOpts :: [String]
    -- ^ Additional synonymous long options may be specified here. For
    -- instance, if you specified @quiet@ for @longOpt@, you might
    -- want to include @silent@ in this list.

  , argSpec :: ArgSpec
    -- ^ Specifies what arguments, if any, this option takes.
  }
             deriving Show

-- | Specifies 
data ArgSpec = SNoArg | SOptionalArg | SOneArg | STwoArg | SVariableArgs
             deriving Show

data Result = PosArg   { posArg :: String }
              | Stopper
              | Option { label :: String
                       , args :: Args }
              deriving Show

data Args = NoArg
          | OptionalArg  { oArg :: Maybe String }
          | OneArg       { sArg1 :: String }
          | TwoArg       { tArg1 :: String
                         , tArg2 :: String }
          | VariableArgs { vArgs :: [String] }
  deriving Show

data Intersperse = Intersperse | StopOptions

parse :: Intersperse
         -> [OptSpec]
         -> [String]
         -> Either SimpleError [Result]
parse i os ss = toEither $ Prim.parse (map pack ss) (f os) where
  f = case i of Intersperse -> parseIntersperse
                StopOptions -> parseNoIntersperse

parseNoIntersperse :: [OptSpec] -> Parser [Result]
parseNoIntersperse os = do
  let opts = mconcat . map optSpec $ os
  rs <- manyTill opts afterArgs
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

noIntersperseArgs :: Parser [Result]
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

parseIntersperse :: [OptSpec] -> Parser [Result]
parseIntersperse os = do
  let optsAndStopper = foldl1 (<|>) $ optSpecs ++ rest
      rest = [stopperParser, posArgParser]
      optSpecs = map optSpec os
  rs <- manyTill optsAndStopper end
  end <?> error "the end parser should always succeed"
  return rs

stopperParser :: Parser Result
stopperParser = stopper >> return Stopper

posArgParser :: Parser Result
posArgParser = do
  a <- nonOptionPosArg
  return $ PosArg (unpack a)

optSpec :: OptSpec -> Parser Result
optSpec o = let
  lo = makeLongOpt . pack . longOpt $ o
  ss = map makeShortOpt . shortOpts $ o
  ls = map makeLongOpt . map pack . longOpts $ o
  opt = return . Option (longOpt o)
  in case argSpec o of
    SNoArg -> do
      _ <- mixedNoArg lo ls ss
      opt NoArg
    SOptionalArg -> do
      (_, a) <- mixedOptionalArg lo ls ss
      opt (OptionalArg . fmap unpack $ a)
    SOneArg -> do
      (_, a) <- mixedOneArg lo ls ss
      opt (OneArg . unpack $ a)
    STwoArg -> do
      (_, a1, a2) <- mixedTwoArg lo ls ss
      opt (TwoArg (unpack a1) (unpack a2))
    SVariableArgs -> do
      (_, as) <- mixedVariableArg lo ls ss
      opt (VariableArgs . map unpack $ as)

