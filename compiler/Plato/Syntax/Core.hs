module Plato.Syntax.Core (
        module Plato.Syntax.Core.Info,
        Term (..),
        Type (..),
        Kind (..),
        Binding (..),
        Command (..),
) where

import Prettyprinter

import Plato.Common.Name
import Plato.Syntax.Core.Info

type Label = Name

data Term
        = -- | Term-level variable @x@
          TmVar !Int NameInfo
        | -- | Application @t t@
          TmApp !Term !Term
        | -- | Abstraction @λx:T.t@
          TmAbs NameInfo !Type !Term
        | -- | Term-type application @t \@T@
          TmTApp !Term !Type
        | -- | Term-type abstraction @λX:K.t@
          TmTAbs NameInfo !Kind !Term
        | -- | Let binding
          TmLet NameInfo !Term !Term
        | -- | Fix combinator @fix t@
          TmFix !Term
        | -- | Record projection @t.i@
          TmProj !Term !Int
        | -- | Record @{l_1:t_1...l_n:t_n}@
          TmRecord ![(Label, Term)]
        | -- | Injection @inj_i[T](t_1...t_n)@
          TmInj Int Type ![Term]
        | -- | Case @case t {l_1->t_1...l_n->t_n}@
          TmCase !Term ![(Label, Term)]
        | TmFold Type
        | TmUnfold Type
        deriving (Eq, Show)

data Type
        = -- | Type-level variable @X@
          TyVar !Int NameInfo
        | -- | Type of functions  @T->T@
          TyFun !Type !Type
        | -- | Universal type @∀X:K.T@
          TyAll NameInfo !Kind !Type
        | -- | Type-type application @T T@
          TyApp !Type !Type
        | -- | Type-type abstraction @ΛX:K.T@
          TyAbs NameInfo !Kind !Type
        | -- | Recursive type @μX:K.T@
          TyRec NameInfo !Kind !Type
        | -- | Type of record @{l_1:T_1...l_n:T_n}@
          TyRecord ![(Label, Type)]
        | -- | Sum type @T_1+...+T_n@
          TySum ![Type]
        deriving (Eq, Show)

data Kind
        = -- | Kind of proper types @*@
          KnStar
        | -- | Kind of type operators @K->K@
          KnFun !Kind !Kind
        deriving (Eq, Show)

data Binding
        = NameBind
        | TmVarBind !Type
        | TyVarBind !Kind
        | TmAbbBind !Term !Type
        | TyAbbBind !Type !Kind
        deriving (Show)

data Command
        = Bind NameInfo !Binding
        | Eval !Term
        deriving (Show)

instance Pretty Term
instance Pretty Type
instance Pretty Kind
instance Pretty Command