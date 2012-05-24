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
  , P.Message (Expected, StrMsg, Replaced, UnknownError)
    
    -- * Get command line arguments
  , G.getArgs
  
    -- * The parser
  , parse
  ) where

import qualified System.Console.MultiArg.Prim as P
import qualified System.Console.MultiArg.GetArgs as G
import qualified System.Console.MultiArg.Combinator as C
import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Applicative ( many, (<|>), optional, (*>), (<*) )
import Data.Maybe (catMaybes)

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

looksLikeOpt :: P.Parser ()
looksLikeOpt = do
  b1 <- C.nextLooksLong
  b2 <- C.nextLooksShort
  if b1 || b2
    then return ()
    else fail "next word looks does not look like an option"

parseOptsNoIntersperse :: P.Parser a -> P.Parser [a]
parseOptsNoIntersperse p = many p <* C.notFollowedBy looksLikeOpt


parseStopOpts :: P.Parser a -> (String -> a) -> P.Parser [a]
parseStopOpts optParser p = do
  opts <- parseOptsNoIntersperse optParser
  _ <- optional P.stopper
  args <- many P.nextArg
  return $ opts ++ (map p args)


-- | @parseIntersperse o p@ parses options and positional arguments,
-- where o is a parser that parses options, and p is a function that,
-- when applied to a string, returns the appropriate type.
parseIntersperse :: P.Parser a -> (String -> a) -> P.Parser [a]
parseIntersperse optParser p =
  let pa = P.nonOptionPosArg >>= return . Just . p
      po = optParser >>= return . Just
      ps = P.stopper *> return Nothing
      parser = po <|> ps <|> pa
  in P.manyTill parser P.end >>= return . catMaybes
