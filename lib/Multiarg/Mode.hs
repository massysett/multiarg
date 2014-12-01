-- | Helps you build command-line parsers for programs that have more
-- than one so-called /mode/; examples of such programs include @git@,
-- @darcs@, and @ghc-pkg@.
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

data OptsWithPosArgs a = OptsWithPosArgs
  { opOpts :: [OptSpec a]
  , opIntersperse :: Intersperse
  , opPosArg :: String -> a
  }

instance Functor OptsWithPosArgs where
  fmap f (OptsWithPosArgs o i p) = OptsWithPosArgs (map (fmap f) o) i
    (fmap f p)

-- | Makes a new 'Mode'.
mode
  :: String
  -- ^ Mode name
  -> OptsWithPosArgs a
  -- ^ Mode options
  -> ([a] -> r)
  -- ^ Processes the result of all mode options
  -> Mode r
mode = undefined

parseModeLine
  :: [OptSpec g]
  -- ^ Global options
  -> [Mode a]
  -- ^ All modes
  -> [String]
  -- ^ All command line tokens
  -> Either ModelineError ([g], Maybe a)
parseModeLine = undefined
