module Main where

import           Ast
import           Pretty

main :: IO ()
main = putStrLn $ ppexpr $ Lam
  (Name "a")
  (Lam
    (Name "b")
    (App
      (App (Lam (Name "c") (Var (Name "c"))) (Lam (Name "c") (Var (Name "c"))))
      (Lit (LInt 100))
    )
  )
