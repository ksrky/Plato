{-# LANGUAGE GADTs #-}

module Plato.Syntax.Typing where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import {-# SOURCE #-} Plato.Typing.TrTypes

type TypLName = Located Name
type TypLExpr = Located Expr
type TypLPat = Located Pat
type TypLType = Located Type
type TypLDecl = Located Decl

data Expr
        = VarE TypLName
        | AbsE TypLName (Maybe Type) TypLExpr
        | AppE TypLExpr TypLExpr
        | TAbsE [Name] TypLExpr
        | TAppE TypLExpr [Type]
        | LetE [FuncDecl] TypLExpr
        | ProjE TypLExpr TypLName
        | RecordE [(TypLName, TypLExpr)]
        | CaseE TypLExpr (Maybe Type) [(TypLPat, TypLExpr)]
        | TagE TypLName [TypLExpr] (Maybe Type)
        | AnnE TypLExpr TypLType {-Sigma-}
        | LetrecE FuncDecl TypLExpr
        deriving (Eq, Show)

data Pat
        = VarP TypLName
        | ConP TypLName [Located Pat]
        | WildP
        deriving (Eq, Show)

data Type
        = VarT (Located TyVar)
        | ConT TypLName
        | ArrT TypLType TypLType
        | AllT [Located TyVar] TypLType {- Rho -}
        | AbsT TypLName TypLType
        | AppT TypLType TypLType
        | RecT TypLName TypLType
        | RecordT [(TypLName, TypLType)]
        | SumT [(TypLName, [TypLType])]
        | MetaT MetaTv
        deriving (Eq, Show)

data Kind
        = VarK Name
        | StarK
        | ArrK Kind Kind
        deriving (Eq, Show)

data FuncDecl = FD TypLName TypLExpr TypLType deriving (Eq, Show)

data Decl
        = TypeD TypLName TypLType
        | VarD TypLName TypLType
        | FuncD FuncDecl
        deriving (Eq, Show)

data Decls = Decls
        { mmodule :: Maybe (Located ModuleName)
        , imports :: [Located ModuleName]
        , decls :: [TypLDecl]
        , body :: [FuncDecl]
        }
        deriving (Eq, Show)
