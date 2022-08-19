module Plato.Abstract.Syntax where

import Plato.Common.Info
import Plato.Common.Name

data Expr
        = VarExpr Info Name
        | AppExpr Expr Expr
        | TAppExpr Info Expr [Type]
        | LamExpr Info [Name] Expr
        | LetExpr Info [Decl] Expr
        | CaseExpr Info Expr [(Pat, Expr)]
        deriving (Eq, Show)

data Pat
        = ConPat Info Name [Pat]
        | VarPat Info Name
        | WildPat Info
        deriving (Eq, Show)

data Type
        = ConType Info Name
        | VarType Info Name
        | AppType Type Type
        | ArrType Info Type Type
        | AllType Info [Name] Type
        deriving (Eq, Show)

data Decl
        = FuncDecl Info Name [Name] Expr
        | FuncTyDecl Info Name Type
        deriving (Eq, Show)

data TopDecl
        = DataDecl Info Name [Name] [(Info, Name, [Type])]
        | TypeDecl Info Name [Name] Type
        | Decl Decl
        deriving (Eq, Show)

newtype ImpDecl = ImpDecl ModuleName deriving (Eq, Show)