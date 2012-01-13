{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Parser primitives. These are the only functions that have access
-- to the internals of the parser.
module System.Console.MultiArg.Prim where

import qualified System.Console.MultiArg.Error as E
import System.Console.MultiArg.Option
  (ShortOpt,
    unShortOpt,
    LongOpt,
    unLongOpt )
import System.Console.MultiArg.TextNonEmpty
  ( TextNonEmpty ( TextNonEmpty ) )
import Control.Applicative ( Applicative,
                             Alternative ( empty, (<|>)) )
import qualified Control.Applicative
import Control.Monad.Exception.Synchronous
  ( ExceptionalT (ExceptionalT), runExceptionalT, throwT,
    Exceptional(Success, Exception) )
import Control.Monad.Trans.State.Lazy
  ( State, state, get, runState, put,
    modify, runStateT )
import Data.Functor.Identity ( runIdentity )
import Data.Text ( Text, pack, isPrefixOf, cons )
import qualified Data.Text as X
import qualified Data.Set as Set
import Data.Set ( Set )
import Control.Monad ( when, liftM,
                       MonadPlus(mzero, mplus) )
import Control.Monad.Trans.Class ( lift )
import Test.QuickCheck ( Arbitrary ( arbitrary ),
                         suchThat,
                         CoArbitrary ( coarbitrary ),
                         (><), choose )
import System.Console.MultiArg.QuickCheckHelpers ( WText(WText) )
import Test.QuickCheck.Gen ( oneof )
import Text.Printf ( printf )
import Data.Maybe ( isNothing, isJust, fromJust, fromMaybe )
import Data.List ( find )
import Data.Monoid ( Monoid ( mempty, mappend ) )

textHead :: Text -> Maybe (Char, Text)
textHead t = case X.null t of
  True -> Nothing
  False -> Just (X.head t, X.tail t)

toTextNonEmpty :: Text -> Maybe TextNonEmpty
toTextNonEmpty t = case textHead t of
  Nothing -> Nothing
  (Just (c, r)) -> Just $ TextNonEmpty c r

data ParseSt s = ParseSt { pendingShort :: Maybe TextNonEmpty
                         , remaining :: [Text]
                         , sawStopper :: Bool
                         , userState :: s
                         , counter :: Int
                         } deriving (Show, Eq)

data Result e a = Bad e | Good a

data ParserT s e m a =
  ParserT { runParserT :: ParseSt s -> m (Result e a, ParseSt s) }

parserBind ::
  (Monad m)
  => ParserT s e m a
  -> (a -> ParserT s e m b)
  -> ParserT s e m b
parserBind (ParserT l) f = ParserT $ \s ->
  l s >>= \(r, s') ->
  case r of
    (Bad e) -> return (Bad e, s')
    (Good g) ->
      let (ParserT fr) = f g
      in fr s'

parserReturn ::
  (Monad m)
  => a
  -> ParserT s e m a
parserReturn a = ParserT $ \s ->
  return (Good a, s)

parserFail ::
  (E.Error e, Monad m)
  => String
  -> ParserT s e m a
parserFail e = ParserT $ \s ->
  return (Bad (E.parseErr E.ExpOtherFailure
               (E.SawTextError (pack e))), s)

instance (E.Error e, Monad m) => Monad (ParserT s e m) where
  (>>=) = parserBind
  return = parserReturn
  fail = parserFail
