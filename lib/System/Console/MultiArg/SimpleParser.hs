-- | A simple command line parser that can parse options that take an
-- optional argument, one or two arguments, or a variable number of
-- arguments. For sample code that uses this parser, see
-- "System.Console.MultiArg.SampleParser".
module System.Console.MultiArg.SimpleParser (
  -- * Interspersion control
  Intersperse (Intersperse, StopOptions)
  
  -- * Option specifications
  , C.OptSpec (OptSpec, longOpts, shortOpts, argSpec)
  , C.ArgSpec (NoArg, OptionalArg, OneArg, TwoArg, VariableArg)
    
    -- * Exceptions
  , Ex.Exceptional (Exception, Success)
  , P.Error (Error)
  , P.Message (Expected, FromFail, Replaced, UnknownError)
    
    -- * Get command line arguments
  , G.getArgs
  
    -- * The parser
  , parse
  ) where

import qualified System.Console.MultiArg.Prim as P
import qualified System.Console.MultiArg.GetArgs as G
import qualified System.Console.MultiArg.Combinator as C
import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Applicative ( many, (<|>), optional, (*>),
                             (<$>))

-- | What to do after encountering the first non-option,
-- non-option-argument word on the command line? In either case, no
-- more options are parsed after a stopper.
data Intersperse =
  Intersperse
  -- ^ Additional options are allowed on the command line after
  -- encountering the first positional argument. For example, if @a@
  -- and @b@ are options, in the command line @-a posarg -b@, @b@ will
  -- be parsed as an option. If @b@ is /not/ an option and the same
  -- command line is entered, then @-b@ will result in an error
  -- because @-b@ starts with a hyphen and therefore \"looks like\" an
  -- option.
  
  | StopOptions
    -- ^ No additional options will be parsed after encountering the
    -- first positional argument. For example, if @a@ and @b@ are
    -- options, in the command line @-a posarg -b@, @b@ will be parsed
    -- as a positional argument rather than as an option.

-- | Parse a command line. 
parse ::
  Intersperse
  -> [C.OptSpec a]
  -> (String -> a)
  -> [String]
  -- ^ The command line to parse. This function correctly handles
  -- Unicode strings; however, because 'System.Environment.getArgs'
  -- does not always correctly handle Unicode strings, consult the
  -- documentation in 'System.Console.MultiArg.GetArgs' and consider
  -- using the functions in there if there is any chance that you will
  -- be parsing command lines that have non-ASCII strings.

  -> Ex.Exceptional P.Error [a]
parse i os p as =
  let optParser = C.parseOption os
      parser = case i of
        Intersperse -> parseIntersperse optParser p
        StopOptions -> parseStopOpts optParser p
  in P.parse as parser

parseStopOpts :: P.Parser a -> (String -> a) -> P.Parser [a]
parseStopOpts optParser p = do
  opts <- many optParser
  _ <- optional P.stopper
  args <- many P.nextArg
  return $ opts ++ (map p args)


parseFirsts :: P.Parser a -> (String -> a) -> P.Parser [a]
parseFirsts optParser p =  do
  let beforeStopperPosArg = p <$> P.nonOptionPosArg
  many (optParser <|> beforeStopperPosArg)

parseIntersperse :: P.Parser a -> (String -> a) -> P.Parser [a]
parseIntersperse optParser p = do
  firsts <- parseFirsts optParser p
  let afterStop = P.stopper *> many P.nextArg
  after <- optional afterStop
  let more = case after of
        Nothing -> []
        Just m -> m
  P.end
  return $ firsts ++ (map p more)
