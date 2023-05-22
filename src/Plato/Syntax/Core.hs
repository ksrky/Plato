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
        = -- | Term-level variable
          TmVar !Int NameInfo
        | -- | Application
          TmApp Term Term
        | -- | Abstraction
          TmAbs NameInfo Type Term
        | -- | Type application
          TmTApp Term Type
        | -- | Type abstraction
          TmTAbs NameInfo Kind Term
        | -- | Let binding
          TmLet NameInfo Term Term
        | -- | Fix combinator
          TmFix Term
        | -- | Record projection
          TmProj Term Int
        | -- | Record
          TmRecord [(Label, Term)]
        | -- | Injection to variants
          TmInj Int Term Type
        | -- | Case tree
          TmCase Term [(Label, Term)]
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