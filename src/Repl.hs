module Repl where

import           Ast
import           Pretty
import           Parse                          ( pExprOuter )
import           Eval
import           DisplayColor
import           System.IO
import           System.Console.Haskeline
import           Text.Printf
import           Text.Megaparsec                ( parse
                                                , eof
                                                , errorBundlePretty
                                                , optional
                                                )
import           Control.Monad                  ( unless )
import           Control.Exception              ( try
                                                , IOException
                                                )
import           Control.Monad.IO.Class
import           Data.Maybe                     ( fromJust )
import           Data.Tuple                     ( swap )
import           Data.List                      ( intercalate
                                                , isPrefixOf
                                                )
import qualified Data.HashMap                  as HM


prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

quit = [":quit", ":q"]
help = [":help", ":h"]

indent = "  "

formatHelp :: [String] -> String -> String
formatHelp cs = printf
  "%s%s - %s"
  indent
  (intercalate " or " (map (\c -> '`' : displayColor [Bold, Green] c ++ "`") cs)
  )

parseStr string fn = case parse (pExprOuter <* eof) "<string>" string of
  Left  err -> outputStrLn (errorBundlePretty err)
  Right ast -> fn ast

repl :: InputT IO ()
repl = do
  input <- getInputLine $ displayColor [Bold, Blue] "\955> "

  case input of
    Just c | ":parse" `isPrefixOf` c -> parseStr (drop 6 c) (outputStrLn . show)
    Just c | c `elem` quit -> outputStrLn $ displayColor [Green] "bye!"
    Just c | c `elem` help -> outputStrLn $ printf
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
    Just "" -> outputStr ""
    Just c  -> parseStr
      c
      (\ast -> do
        res <- liftIO $ eval HM.empty ast
        do
          case res of
            Left  e          -> outputStrLn e
            Right expression -> outputStrLn $ show expression
      )
    Nothing -> return ()

  unless (maybe False (`elem` quit) input) repl

logo =
  "  ____        _      _      _                     _         _\n\
\ / __ \\      (_)    | |    | |                   | |       | |\n\
\| |  | |_   _ _  ___| | __ | |     __ _ _ __ ___ | |__   __| | __ _\n\
\| |  | | | | | |/ __| |/ / | |    / _` | '_ ` _ \\| '_ \\ / _` |/ _` |\n\
\| |__| | |_| | | (__|   <  | |___| (_| | | | | | | |_) | (_| | (_| |\n\
\ \\___\\_\\\\__,_|_|\\___|_|\\_\\ |______\\__,_|_| |_| |_|_.__/ \\__,_|\\__,_|"
