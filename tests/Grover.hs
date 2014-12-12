{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Testing of mode parsers.

module Grover where

import Control.Applicative
import Multiarg.Mode
import Text.Read (readMaybe)

data Global
  = Help
  | Verbose Int
  -- ^ The Int indicates the desired level of verbosity.
  | Version
  deriving (Eq, Ord, Show)

data GroverOpt a
  = Zero
  | Single a
  | Double a a
  | Triple a a a
  | PosArg String
  deriving (Eq, Ord, Show)

instance Functor GroverOpt where
  fmap f g = case g of
    Zero -> Zero
    Single a -> Single (f a)
    Double a b -> Double (f a) (f b)
    Triple a b c -> Triple (f a) (f b) (f c)
    PosArg a -> PosArg a

globalOptSpecs :: [OptSpec (Either String Global)]
globalOptSpecs =
  [ optSpec "h" ["help"] . ZeroArg . return $ Help
  , optSpec "v" ["verbose"] . OneArg $ \s ->
    Verbose <$> readErr s
  , optSpec "" ["version"] . ZeroArg . return $ Version
  ]

modeOptSpecs :: Read a => [OptSpec (Either String (GroverOpt a))]
modeOptSpecs =
  [ optSpec "z" ["zero"] . ZeroArg . Right $ Zero
  , optSpec "s" ["single"] . OneArg $ \s -> Single <$> readErr s

  , optSpec "d" ["double"] . TwoArg $ \s1 s2 ->
      Double <$> readErr s1 <*> readErr s2

  , optSpec "t" ["triple"] . ThreeArg $ \s1 s2 s3 ->
      Triple <$> readErr s1 <*> readErr s2 <*> readErr s3
  ]

data Result
  = Ints [Either String (GroverOpt Int)]
  | Strings [Either String (GroverOpt String)]
  | Maybes [Either String (GroverOpt (Maybe Int))]
  deriving (Eq, Ord, Show)

modes :: [Mode Result]
modes =
  [ mode "int" modeOptSpecs (return . PosArg) Ints
  , mode "string" modeOptSpecs (return . PosArg) Strings
  , mode "maybe" modeOptSpecs (return . PosArg) Maybes
  ]

readErr :: Read a => String -> Either String a
readErr s = case readMaybe s of
  Nothing -> Left $ "could not read value: " ++ s
  Just a -> Right a

parseGrover
  :: [String]
  -> Either ModelineError ([Either String Global], Maybe Result)
parseGrover = parseModeLine globalOptSpecs modes
