module Repl where

import           System.IO
import           Ast
import           Pretty
import           Parse
import           Text.Megaparsec
import           Text.Printf
import           Control.Monad                  ( unless )
import           Data.Maybe                     ( fromJust )
import           Data.Tuple                     ( swap )
import           Data.List                      ( intercalate )


prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

data Font = Bold | Reset | Red | Green | Blue deriving Eq
table =
  [ (Bold , "\ESC[1m")
  , (Reset, "\ESC[0m")
  , (Blue , "\ESC[94m")
  , (Red  , "\ESC[91m")
  , (Green, "\ESC[92m")
  ]

getEscape font = fromJust $ lookup font table

display :: [Font] -> String -> String
display font text = foldl1 (++) (map getEscape font) ++ text ++ getEscape Reset

quit = [":quit", ":q"]
help = [":help", ":h"]

indent = "  "

formatHelp :: [String] -> String -> String
formatHelp cs = printf
  "%s%s - %s"
  indent
  (intercalate " or " (map (\c -> '`' : display [Bold, Green] c ++ "`") cs))

repl :: IO ()
repl = do
  code <- prompt $ display [Bold, Blue] "lambda> "

  case code of
    c | c `elem` quit -> putStrLn $ display [Green] "bye!"
    c | c `elem` help -> putStrLn $ printf
      "\n%s\n\n%s\n"
      logo
      (intercalate
        "\n"
        [ printf " %s - %s"
                 (display [Bold, Blue] "quick-lambda")
                 (display [Blue] "A lambda calculus interpreter.")
        , formatHelp help "show this help message"
        , formatHelp quit "exit the interpreter"
        ]
      )
    _ -> case parse (pExpr <* eof) "<string>" code of
      Left  err -> putStrLn (errorBundlePretty err)
      Right ast -> print ast

  unless (code `elem` quit) repl

logo =
  "  ____        _      _      _                     _         _\n\
\ / __ \\      (_)    | |    | |                   | |       | |\n\
\| |  | |_   _ _  ___| | __ | |     __ _ _ __ ___ | |__   __| | __ _\n\
\| |  | | | | | |/ __| |/ / | |    / _` | '_ ` _ \\| '_ \\ / _` |/ _` |\n\
\| |__| | |_| | | (__|   <  | |___| (_| | | | | | | |_) | (_| | (_| |\n\
\ \\___\\_\\\\__,_|_|\\___|_|\\_\\ |______\\__,_|_| |_| |_|_.__/ \\__,_|\\__,_|"
