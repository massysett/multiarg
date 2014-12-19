module Multiarg.Types
  ( ArgSpec(..)
  , OptSpec(..)
  , optSpec
  , ShortName
  , shortNameToChar
  , shortName
  , LongName
  , longNameToString
  , longName
  , Token(..)
  , OptName(..)
  , OptArg(..)
  , ShortTail(..)
  , isLong
  , isShort
  , tokenToOptArg
  , splitShortTail
  ) where

-- | Specifies how many arguments an option takes.
data ArgSpec a
  = ZeroArg a
  -- ^ This option takes no arguments
  | OneArg (String -> a)
  -- ^ This option takes one argument
  | TwoArg (String -> String -> a)
  -- ^ This option takes two arguments
  | ThreeArg (String -> String -> String -> a)
  -- ^ This option takes three arguments

instance Functor ArgSpec where
  fmap f (ZeroArg a) = ZeroArg (f a)
  fmap f (OneArg g) = OneArg $ \a -> f (g a)
  fmap f (TwoArg g) = TwoArg $ \a b -> f (g a b)
  fmap f (ThreeArg g) = ThreeArg $ \a b c -> f (g a b c)

instance Show (ArgSpec a) where
  show (ZeroArg _) = "ZeroArg"
  show (OneArg _) = "OneArg"
  show (TwoArg _) = "TwoArg"
  show (ThreeArg _) = "ThreeArg"

-- | Specifies an option.  Typically you will use 'optSpec' to create
-- an 'OptSpec' rather than using the constructor directly.
data OptSpec a = OptSpec [ShortName] [LongName] (ArgSpec a)
  deriving Show

instance Functor OptSpec where
  fmap f (OptSpec s l p) = OptSpec s l (fmap f p)

-- | Creates an 'OptSpec'.
optSpec
  :: [Char]
  -- ^ There is one character for each desired short option name.
  -- Each of these characters may not be a hyphen; otherwise,
  -- 'optSpec' will apply 'error'.

  -> [String]
  -- ^ There is one string for each desired long option name.  Each
  -- string:
  --
  -- * cannot be empty;
  --
  -- * must not begin with a hyphen; and
  --
  -- * must not contain an equal sign.
  --
  -- Otherwise, 'optSpec' will apply 'error'.

  -> ArgSpec a
  -- ^ How many arguments this option takes.
  -> OptSpec a
optSpec ss ls = OptSpec (map mkShort ss) (map mkLong ls)
  where
    mkShort s = case shortName s of
      Nothing -> error $ "invalid short option name: " ++ [s]
      Just n -> n
    mkLong s = case longName s of
      Nothing -> error $ "invalid long option name: " ++ s
      Just n -> n


-- | A short option name.
newtype ShortName = ShortName { shortNameToChar ::  Char }
  deriving (Eq, Ord, Show)

-- | A long option name.
newtype LongName = LongName { longNameToString :: String }
  deriving (Eq, Ord, Show)

-- | Creates a short option name.  Any character other than a single
-- hyphen will succeed.
shortName :: Char -> Maybe ShortName
shortName '-' = Nothing
shortName x = Just $ ShortName x

-- | Creates a long option name.  The string may not be empty, and the
-- first character may not be a hyphen.  In addition, no character may
-- be an equal sign.
longName :: String -> Maybe LongName
longName s = case s of
  [] -> Nothing
  '-':_ -> Nothing
  xs | '=' `elem` xs -> Nothing
     | otherwise -> Just $ LongName xs

-- | The name of an option (either short or long).
newtype OptName = OptName (Either ShortName LongName)
  deriving (Eq, Ord, Show)

-- | A token supplied by the user on the command line.
newtype Token = Token String
  deriving (Eq, Ord, Show)

-- | An option argument.
newtype OptArg = OptArg { optArgToString :: String }
  deriving (Eq, Ord, Show)

-- | Is this token an input for a long option?
isLong
  :: Token
  -> Maybe (LongName, Maybe OptArg)
  -- ^ Nothing if the option does not begin with a double dash and is
  -- not at least three characters long.  Otherwise, returns the
  -- characters following the double dash to the left of any equal
  -- sign.  The Maybe in the tuple is Nothing if there is no equal
  -- sign, or Just followed by characters following the equal sign if
  -- there is one.
isLong (Token ('-':'-':[])) = Nothing
isLong (Token ('-':'-':xs)) = Just (LongName optName, arg)
  where
    (optName, end) = span (/= '=') xs
    arg = case end of
      [] -> Nothing
      _:rs -> Just . OptArg $ rs
isLong _ = Nothing

-- | Characters after the first character in a short option; for
-- instance, if the user supplies @-afoobar@, then this will be
-- @foobar@.
newtype ShortTail = ShortTail String
  deriving (Eq, Ord, Show)

-- | Is this the input token for a short argument?
isShort
  :: Token
  -> Maybe (ShortName, ShortTail)
isShort (Token ('-':'-':_)) = Nothing
isShort (Token ('-':[])) = Nothing
isShort (Token ('-':x:xs)) = Just (ShortName x, ShortTail xs)
isShort _ = Nothing

tokenToOptArg :: Token -> OptArg
tokenToOptArg (Token t) = OptArg t

-- | If possible, splits a ShortTail into a short option name and a
-- remaining tail.
splitShortTail :: ShortTail -> Maybe (ShortName, ShortTail)
splitShortTail (ShortTail s) = case s of
  [] -> Nothing
  x:xs -> Just (ShortName x, ShortTail xs)
