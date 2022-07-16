module Plato.Syntax.AST where

import Plato.Common.Info
import Plato.Common.Name (Name)

data Expr
        = VarExpr Info Name [Expr]
        | ConExpr Info Name [Expr]
        | FloatExpr Float
        | StringExpr String
        | LamExpr Info Name Expr
        | LetExpr Info [Decl] Expr
        | CaseExpr Info Expr [(Expr, Expr, Info)]
        deriving (Eq, Show)

data Decl
        = FuncDecl Info Name Expr
        | FuncTyDecl Info Name Type
        deriving (Eq, Show)

data TopDecl
        = DataDecl Info Name [Name] [(Name, [Type])]
        | TypeDecl Info Name [Name] Type
        | Decl Decl
        deriving (Eq, Show)

data Type
        = ConType Info Name
        | VarType Info Name
        | AppType Type Type
        | FunType Info Type Type
        | AllType Name Type
        deriving (Eq, Show)
