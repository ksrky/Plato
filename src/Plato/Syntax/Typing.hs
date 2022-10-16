module Plato.Syntax.Typing where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc

type TypLName = Located Name
type TypLExpr = Located Expr
type TypLPat = Located Pat
type TypLType = Located Type
type TypLDecl = Located Decl

data Expr
        = VarExpr TypLName
        | AbsExpr TypLName (Maybe Type) TypLExpr
        | AppExpr TypLExpr TypLExpr
        | TAbsExpr TypLName (Maybe Kind) TypLExpr
        | TAppExpr TypLExpr TypLType
        | LetExpr TypLName TypLType TypLExpr TypLExpr
        | ProjExpr TypLExpr TypLName
        | RecordExpr [(TypLName, TypLExpr)]
        | CaseExpr TypLExpr (Maybe Type) [(TypLPat, TypLExpr)]
        | TagExpr TypLName [TypLExpr] (Maybe Type)
        deriving (Eq, Show)

data Pat
        = ConPat TypLName [Pat]
        | AnyPat (Maybe TypLName)
        deriving (Eq, Show)

data Type
        = VarType TypLName
        | ArrType TypLType TypLType
        | AllType TypLName (Maybe Kind) TypLType
        | AbsType TypLName (Maybe Kind) TypLType
        | AppType TypLType TypLType
        | RecType TypLName TypLType
        | RecordType [(TypLName, TypLType)]
        | SumType [(TypLName, [TypLType])]
        deriving (Eq, Show)

data Kind
        = VarKind Name
        | StarKind
        | ArrKind Kind Kind
        deriving (Eq, Show)

data Decl
        = TypeDecl TypLName TypLType
        | VarDecl TypLName TypLType
        | FuncDecl TypLName TypLExpr TypLType
        deriving (Eq, Show)

data Decls = Decls
        { imports :: [Located ModuleName]
        , decls :: [TypLDecl]
        , body :: (TypLExpr, TypLType)
        }
        deriving (Eq, Show)
