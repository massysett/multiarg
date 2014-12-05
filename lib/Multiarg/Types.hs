module Multiarg.Types where

import Multiarg.Maddash

data OptSpec a = OptSpec
  { shorts :: [ShortName]
  , longs :: [LongName]
  , spec :: ArgSpec a
  } deriving Show

instance Functor OptSpec where
  fmap f (OptSpec s l p) = OptSpec s l (fmap f p)

-- | Creates an OptSpec.
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
  -- * must not contain an equal sign;
  --
  -- otherwise, 'optSpec' will apply 'error'.

  -> ArgSpec a
  -> OptSpec a
optSpec ss ls = OptSpec (map mkShort ss) (map mkLong ls)
  where
    mkShort s = case shortName s of
      Nothing -> error $ "invalid short option name: " ++ [s]
      Just n -> n
    mkLong s = case longName s of
      Nothing -> error $ "invalid long option name: " ++ s
      Just n -> n
