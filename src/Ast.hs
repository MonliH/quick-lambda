{-# LANGUAGE DeriveGeneric #-}

module Ast where
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

newtype Name = Name String deriving (Show, Eq, Ord, Generic)
instance Hashable Name

data BinOp = Add | Mul | Sub deriving (Show)

data Expr = Var Name
          | App Expr Expr
          | Lam Name Expr
          | BinOp BinOp Expr Expr
          | Lit Lit deriving (Show)

data Lit = LInt Int
         | LBool Bool deriving (Show)
