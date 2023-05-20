module Plato.Syntax.Core (
        module Plato.Syntax.Core.Info,
        Term (..),
        Type (..),
        Kind (..),
        Binding (..),
        Command (..),
) where

import Plato.Common.Name
import Plato.Syntax.Core.Info

type Label = Name

data Term
        = TmVar !Int NameInfo
        | TmApp Term Term
        | TmAbs NameInfo Type Term
        | TmTApp Term Type
        | TmTAbs NameInfo Kind Term
        | TmLet NameInfo Term Term
        | TmFix Term
        | TmProj Term Int
        | TmRecord [(Label, Term)]
        | TmInj Int Term Type
        deriving (Eq, Show)

data Type
        = TyVar !Int NameInfo
        | TyFun Type Type
        | TyAll NameInfo Kind Type
        | TyApp Type Type
        | TyAbs NameInfo Kind Type
        | TyRec NameInfo Kind Type
        | TyRecord [(Label, Type)]
        | TySum [Type]
        deriving (Eq, Show)

data Kind = KnStar | KnFun Kind Kind
        deriving (Eq, Show)

data Binding
        = NameBind
        | TmVarBind Type
        | TyVarBind Kind
        | TmAbbBind Term Type
        | TyAbbBind Type Kind

data Command
        = Bind NameInfo Binding
        | Eval Term