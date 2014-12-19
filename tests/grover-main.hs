module Main where

import Grover
import System.Environment

main :: IO ()
main = do
  as <- getArgs
  putStrLn . show $ parseGrover as
