module Plato.Syntax.Typing where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import {-# SOURCE #-} Plato.Typing.Types

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
        | LetExpr FuncDecl TypLExpr
        | ProjExpr TypLExpr TypLName
        | RecordExpr [(TypLName, TypLExpr)]
        | CaseExpr TypLExpr (Maybe Type) [(TypLPat, TypLExpr)]
        | TagExpr TypLName [TypLExpr] (Maybe Type)
        | AnnExpr TypLExpr TypLType
        deriving (Eq, Show)

data Pat
        = VarP TypLName
        | ConP TypLName [Pat]
        | WildP
        deriving (Eq, Show)

data Type
        = VarType (Located TyVar)
        | ConType TypLName
        | ArrType TypLType TypLType
        | AllType [Located TyVar] (Located Type {- Rho -})
        | AbsType TypLName TypLType
        | AppType TypLType TypLType
        | RecType TypLName TypLType
        | RecordType [(TypLName, TypLType)]
        | SumType [(TypLName, [TypLType])]
        | MetaType MetaTv
        deriving (Eq, Show)

data Kind
        = VarKind Name
        | StarKind
        | ArrKind Kind Kind
        deriving (Eq, Show)

data FuncDecl = FD TypLName TypLExpr TypLType deriving (Eq, Show)

data Decl
        = TypeDecl TypLName TypLType
        | VarDecl TypLName TypLType
        | FuncDecl FuncDecl
        deriving (Eq, Show)

data Decls = Decls
        { imports :: [Located ModuleName]
        , decls :: [TypLDecl]
        , body :: FuncDecl
        }
        deriving (Eq, Show)
