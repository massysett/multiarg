module Multiarg.Complete where

import Data.Bifunctor
import qualified Multiarg.Maddash as Maddash
import qualified Multiarg.Limeline as Limeline

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
  deriving (Eq, Ord, Show)

data OptSpec a = OptSpec
  { shorts :: [Char]
  , longs :: [String]
  , spec :: Maddash.ArgSpec a
  } deriving Show

instance Functor OptSpec where
  fmap f (OptSpec s l p) = OptSpec s l (fmap f p)

data Opts s a = Opts
  { oShortcuts :: [OptSpec s]
  , oOptions :: [OptSpec a]
  } deriving Show

instance Functor (Opts s) where
  fmap f (Opts s o) = Opts s (map (fmap f) o)

instance Bifunctor Opts where
  bimap fa fb (Opts as bs) = Opts (map (fmap fa) as) (map (fmap fb) bs)

data OptsWithPosArgs s a = OptsWithPosArgs
  { opOpts :: Opts s a
  , opIntersperse :: Intersperse
  , opPosArg :: String -> Either Maddash.Diagnostic a
  }

instance Functor (OptsWithPosArgs s) where
  fmap f (OptsWithPosArgs o i p) = OptsWithPosArgs (fmap f o) i
    (\s -> fmap f (p s))

instance Bifunctor OptsWithPosArgs where
  bimap fa fb (OptsWithPosArgs o i p) = OptsWithPosArgs (bimap fa fb o)
    i (\s -> fmap fb (p s))

data Mode s r = Mode
  { mModeName :: String
  , mOpts :: OptsWithPosArgs s r
  }

instance Functor (Mode s) where
  fmap f (Mode n o) = Mode n (fmap f o)

instance Bifunctor Mode where
  bimap fa fb (Mode s o) = Mode s (bimap fa fb o)
