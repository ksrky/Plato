module Plato.Syntax.Core where

import Plato.Common.GlbName
import Plato.Common.SrcLoc

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Term
        = TmVar Int Int
        | TmApp Term Term
        | TmAbs GlbName Ty Term
        | TmTApp Term Ty
        | TmTAbs GlbName Term
        | TmLet GlbName Term Term
        | TmFix Term
        | TmFold Ty
        | TmUnfold Ty
        | TmProj Term GlbName
        | TmRecord [(GlbName, Term)]
        | TmTag GlbName [Term] Ty
        | TmCase Term [(GlbName, (Int, Term))]
        deriving (Eq, Show)

data Ty
        = TyVar Int Int
        | TyArr Ty Ty
        | TyAll GlbName Kind Ty
        | TyAbs GlbName Kind Ty
        | TyApp Ty Ty
        | TyRec GlbName Kind Ty
        | TyRecord [(GlbName, Ty)]
        | TyVariant [(GlbName, [Ty])]
        deriving (Eq, Show)

data Kind = KnStar | KnArr Kind Kind deriving (Eq, Show)

data Binding
        = NameBind
        | VarBind (Located Ty)
        | TyVarBind Kind
        | TmAbbBind (Located Term) (Located Ty)
        | TyAbbBind (Located Ty) Kind
        deriving (Eq, Show)

-- data Command
--         = Import ModuleName
--         | Bind GlbName Binding
--         | Eval (Located Term)
--         deriving (Eq, Show)

data Module = Module
        { moduleBind :: [(GlbName, Binding)]
        , moduleEval :: [Located Term]
        }
        deriving (Eq, Show)