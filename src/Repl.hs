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
    Just c | c `elem` help ->
      outputStrLn
        $  "\n"
        ++ intercalate
             "\n"
             [ heading
             , formatHelp help "show this help message"
             , formatHelp quit "exit the interpreter"
             ]
        ++ "\n"
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

heading = printf " %s (%s) - %s"
                 (displayColor [Bold, Blue] "quick-lambda")
                 (displayColor [Green] version)
                 (displayColor [Blue] "A lambda calculus interpreter.")

version = "0.0.1"
