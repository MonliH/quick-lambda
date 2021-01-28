module Main where

import           Repl                           ( repl )
import           System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings repl
