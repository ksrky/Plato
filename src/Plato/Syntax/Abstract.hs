module Plato.Syntax.Abstract where

import Plato.Common.Info
import Plato.Common.Name (Name)

data Expr
        = VarExpr Info Name [Expr]
        | ConExpr Info Name [Expr]
        | FloatExpr Info Float
        | StringExpr Info String
        | LamExpr Info [Name] Expr
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
        | ArrType Info Type Type
        | AllType Info [Name] Type
        deriving (Eq, Show)

getInfo :: Expr -> Info
getInfo (VarExpr fi _ _) = fi
getInfo (ConExpr fi _ _) = fi
getInfo (FloatExpr fi _) = fi
getInfo (StringExpr fi _) = fi
getInfo (LamExpr fi _ _) = fi
getInfo (LetExpr fi _ _) = fi
getInfo (CaseExpr fi _ _) = fi
