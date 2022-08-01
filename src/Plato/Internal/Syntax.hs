module Plato.Internal.Syntax where

import Plato.Common.Info (Info)
import Plato.Common.Name (Name)

data Expr
        = VarExpr Info Name [Expr]
        | ConExpr Info Name [Expr]
        | FloatExpr Info Float
        | StringExpr Info String
        | LamExpr Info Name Expr
        | LetExpr Info Decl Expr
        | CaseExpr Info Expr [(Info, Expr, Expr)]
        | TagExpr Info Name [Expr]
        deriving (Eq, Show)

data Type
        = VarType Info Name
        | AppType Type Type
        | ArrType Info Type Type
        | AbsType Info Name Type
        | AllType Info Name Type
        | SumType [(Info, Name, [Type])]
        deriving (Eq, Show)

data Decl
        = TypeDecl Info Name Type
        | FuncDecl Info Name Expr Type
        deriving (Eq, Show)

class GetInfo a where
        getInfo :: a -> Info

instance GetInfo Expr where
        getInfo (VarExpr fi _ _) = fi
        getInfo (ConExpr fi _ _) = fi
        getInfo (FloatExpr fi _) = fi
        getInfo (StringExpr fi _) = fi
        getInfo (LamExpr fi _ _) = fi
        getInfo (LetExpr fi _ _) = fi
        getInfo (CaseExpr fi _ _) = fi
        getInfo (TagExpr fi _ _) = fi
