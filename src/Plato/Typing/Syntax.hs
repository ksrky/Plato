module Plato.Typing.Syntax where

import Plato.Common.Error (unreachable)
import Plato.Common.Info (Info)
import Plato.Common.Name (ModuleName, Name)

data Expr
        = VarExpr Info Name
        | AppExpr Info Expr Expr
        | TAppExpr Info Expr Type
        | LamExpr Info Name Expr
        | LetExpr Info Decl Expr
        | ProjExpr Info Expr Name
        | RecordExpr Info [(Name, Expr)]
        | CaseExpr Info Expr [(Pat, Expr)]
        | TagExpr Info Name [Expr]
        deriving (Eq, Show)

data Pat
        = ConPat Info Name [Pat]
        | AnyPat Info (Maybe Name)
        deriving (Eq, Show)

data Type
        = VarType Info Name
        | ArrType Info Type Type
        | AllType Info Name Type
        | AbsType Info Name Type
        | AppType Info Type Type
        | RecType Info Name Type
        | RecordType Info [(Name, Type)]
        | SumType [(Info, Name, [Type])]
        deriving (Eq, Show)

data Decl
        = TypeDecl Info Name Type
        | VarDecl Info Name Type
        | FuncDecl Info Name Expr Type
        deriving (Eq, Show)

data Decls = Decls {imports :: [ModuleName], decls :: [Decl], body :: (Expr, Type)} deriving (Eq, Show)
