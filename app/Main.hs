module Main where

import           Repl                           ( repl
                                                , heading
                                                )
import           System.Console.Haskeline       ( defaultSettings
                                                , runInputT
                                                )
import           Text.Printf                    ( printf )

main :: IO ()
main = do
  putStrLn $ printf "%s\n\n%s Enter `:h` for help\n" logo heading
  runInputT defaultSettings repl

logo =
  "  ____        _      _      _                     _         _\n\
\ / __ \\      (_)    | |    | |                   | |       | |\n\
\| |  | |_   _ _  ___| | __ | |     __ _ _ __ ___ | |__   __| | __ _\n\
\| |  | | | | | |/ __| |/ / | |    / _` | '_ ` _ \\| '_ \\ / _` |/ _` |\n\
\| |__| | |_| | | (__|   <  | |___| (_| | | | | | | |_) | (_| | (_| |\n\
\ \\___\\_\\\\__,_|_|\\___|_|\\_\\ |______\\__,_|_| |_| |_|_.__/ \\__,_|\\__,_|"
