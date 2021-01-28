module Repl where

import           System.IO
import           System.Console.Haskeline
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

displayColor :: [Font] -> String -> String
displayColor font text =
  foldl1 (++) (map getEscape font) ++ text ++ getEscape Reset

quit = [":quit", ":q"]
help = [":help", ":h"]

indent = "  "

formatHelp :: [String] -> String -> String
formatHelp cs = printf
  "%s%s - %s"
  indent
  (intercalate " or " (map (\c -> '`' : displayColor [Bold, Green] c ++ "`") cs)
  )

repl :: InputT IO ()
repl = do
  code <- fmap fromJust $ getInputLine $ displayColor [Bold, Blue] "\955> "

  case code of
    c | c `elem` quit -> outputStrLn $ displayColor [Green] "bye!"
    c | c `elem` help -> outputStrLn $ printf
      "\n%s\n\n%s\n"
      logo
      (intercalate
        "\n"
        [ printf " %s - %s"
                 (displayColor [Bold, Blue] "quick-lambda")
                 (displayColor [Blue] "A lambda calculus interpreter.")
        , formatHelp help "show this help message"
        , formatHelp quit "exit the interpreter"
        ]
      )
    "" -> outputStr ""
    _ -> case parse (pExpr <* eof) "<string>" code of
      Left  err -> outputStrLn (errorBundlePretty err)
      Right ast -> outputStrLn $ show ast

  unless (code `elem` quit) repl

logo =
  "  ____        _      _      _                     _         _\n\
\ / __ \\      (_)    | |    | |                   | |       | |\n\
\| |  | |_   _ _  ___| | __ | |     __ _ _ __ ___ | |__   __| | __ _\n\
\| |  | | | | | |/ __| |/ / | |    / _` | '_ ` _ \\| '_ \\ / _` |/ _` |\n\
\| |__| | |_| | | (__|   <  | |___| (_| | | | | | | |_) | (_| | (_| |\n\
\ \\___\\_\\\\__,_|_|\\___|_|\\_\\ |______\\__,_|_| |_| |_|_.__/ \\__,_|\\__,_|"
