module Plato.Syntax.AST where

import Plato.Common.Name (Name)
import Plato.Common.Position (Pos)

data Expr
        = VarExpr Name Pos
        | FloatExpr Float
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
        = ConType Name Pos
        | VarType Name Pos
        | AppType Type Type
        | FunType Type Type Pos
        | AllType Name Type
        deriving (Eq, Show)
