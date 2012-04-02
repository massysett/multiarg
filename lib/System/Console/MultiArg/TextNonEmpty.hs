module System.Console.MultiArg.TextNonEmpty where

import Data.Text ( Text )

data TextNonEmpty = TextNonEmpty Char Text
                    deriving (Show, Eq)
