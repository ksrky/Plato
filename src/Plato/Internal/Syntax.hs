module Plato.Internal.Syntax where

import Plato.Common.Error (unreachable)
import Plato.Common.Info (Info)
import Plato.Common.Name (ModuleName, Name)

data Expr
        = VarExpr Info Name
        | ConExpr Info Name
        | AppExpr Info Expr Expr
        | FloatExpr Info Float
        | StringExpr Info String
        | LamExpr Info Name Expr
        | LetExpr Info Decl Expr
        | CaseExpr Info Expr [(Info, Pat, Expr)]
        | TagExpr Info Name [Expr]
        deriving (Eq, Show)

data Pat
        = ConPat Info Name [Name]
        | AnyPat Info (Maybe Name)
        deriving (Eq, Show)

data Type
        = VarType Info Name
        | ArrType Info Type Type
        | AllType Info Name Type
        | AbsType Info Name Type
        | AppType Info Type Type
        | SumType [(Info, Name, [Type])]
        deriving (Eq, Show)

data Decl
        = TypeDecl Info Name Type
        | FuncDecl Info Name Expr Type
        deriving (Eq, Show)

data Decls = Decls {imports :: [ModuleName], decls :: [Decl]} deriving (Eq, Show)

class GetInfo a where
        getInfo :: a -> Info

instance GetInfo Expr where
        getInfo (VarExpr fi _) = fi
        getInfo (ConExpr fi _) = fi
        getInfo (AppExpr fi _ _) = fi
        getInfo (FloatExpr fi _) = fi
        getInfo (StringExpr fi _) = fi
        getInfo (LamExpr fi _ _) = fi
        getInfo (LetExpr fi _ _) = fi
        getInfo (CaseExpr fi _ _) = fi
        getInfo (TagExpr fi _ _) = fi

instance GetInfo Type where
        getInfo (VarType fi _) = fi
        getInfo (ArrType fi _ _) = fi
        getInfo (AllType fi _ _) = fi
        getInfo (AbsType fi _ _) = fi
        getInfo (AppType fi _ _) = fi
        getInfo (SumType _) = unreachable "SumType does not have Info"