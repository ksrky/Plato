module Plato.Syntax.AST where

data Pos = Pos {line :: Int, col :: Int} deriving (Eq, Show)

type Name = String

data Expr
        = VarExpr Name Pos
        | IntExpr Integer
        | StringExpr String
        | CallExpr Name [Expr] Pos
        | LamExpr Name Expr Pos
        | LetExpr [Decl] Expr Pos
        | CaseExpr Expr [(Expr, Expr, Pos)] Pos
        deriving (Eq, Show)

data Decl
        = FuncDecl Name Expr Pos
        | FuncTyDecl Name Type Pos
        deriving (Eq, Show)

data TopDecl
        = DataDecl Name [Name] [(Name, [Type])] Pos
        | TypeDecl Name [Name] Type Pos
        | Decl Decl
        deriving (Eq, Show)

data Type
        = TyCon Name Pos
        | TyVar Name Pos
        | TyApp Type Type
        | TyFun Type Type Pos
        deriving (Eq, Show)
