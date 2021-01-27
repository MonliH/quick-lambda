module Main where

import           Ast
import           Pretty
import           Parse
import           Text.Megaparsec

main :: IO ()
main = do
  code <- getLine
  case parse (pExpr <* eof) "<string>" code of
    Left  err -> putStrLn (errorBundlePretty err)
    Right ast -> print ast

-- putStrLn $ ppexpr $ Lam
--   (Name "a")
--   (Lam
--     (Name "b")
--     (App
--       (App (Lam (Name "c") (Var (Name "c"))) (Lam (Name "c") (Var (Name "c"))))
--       (Lit (LInt 100))
--     )
--   )
