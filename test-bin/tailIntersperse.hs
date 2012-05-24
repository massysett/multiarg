module Main where

import qualified System.Console.MultiArg as M
import qualified System.Console.MultiArg.SampleParser as S

main :: IO ()
main = S.sampleMain M.Intersperse
