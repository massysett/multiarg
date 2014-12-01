module Multiarg.Mode where

import Multiarg.Maddash
import Multiarg

data GlobalError
  = GlobalInsufficientOptArgs Option
  | GlobalOptsError OptionError
  | ModeNotFound String

data ModeOptionErrors = ModeOptionErrors
  { moeFirst :: [OptionError]
  , moeLast :: Either OptionError Option
  }

data ModelineError
  = ModelineGlobalErorr GlobalError [GlobalError]
      (Maybe (ModeName, ModeOptionErrors))
  | ModelineModeError ModeName ModeOptionErrors

newtype ModeName = ModeName String
  deriving (Eq, Ord, Show)

data Mode a = Mode
  { mModeName :: ModeName
  , mParser :: [Token] -> Either ModeOptionErrors a
  }

instance Functor Mode where
  fmap f (Mode s p) = Mode s (fmap (fmap f) p)

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

data OptsWithPosArgs a = OptsWithPosArgs
  { opOpts :: [OptSpec a]
  , opIntersperse :: Intersperse
  , opPosArg :: String -> a
  }

instance Functor OptsWithPosArgs where
  fmap f (OptsWithPosArgs o i p) = OptsWithPosArgs (map (fmap f) o) i
    (fmap f p)

mode
  :: String
  -- ^ Mode name
  -> OptsWithPosArgs a
  -- ^ Mode options
  -> ([a] -> r)
  -- ^ Processes the result of all mode options
  -> Mode r
mode = undefined

parse
  :: [OptSpec g]
  -- ^ Global options
  -> [Mode a]
  -- ^ All modes
  -> [String]
  -- ^ All command line tokens
  -> Either ModelineError ([g], Maybe a)
parse = undefined
