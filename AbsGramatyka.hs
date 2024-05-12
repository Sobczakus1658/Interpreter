-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language gramatyka.

module AbsGramatyka where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Program = SProgram [Stmt]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FnDef = PFnDef Type Ident [Arg] Block
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Arg = VArg Type Ident | PArg Type Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Block = SBlock [Stmt]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stmt
    = Empty
    | BStmt Block
    | Decl Type [Item]
    | Ass Ident Expr
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | SExp Expr
    | FunExp FnDef
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Item = Init Ident Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type = Int | Str | Bool | Void | Fun [Type] Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
    = EVar Ident
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EApp Ident [Expr]
    | EString String
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | ELam [Arg] Type Block
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data AddOp = Plus | Minus
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MulOp = Times | Div | Mod
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

