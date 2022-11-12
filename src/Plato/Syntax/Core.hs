module Plato.Syntax.Core where

import Plato.Common.Name
import Plato.Common.SrcLoc

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Term
        = TmVar Int Int
        | TmApp Term Term
        | TmAbs Name Ty Term
        | TmTApp Term Ty
        | TmTAbs Name Term
        | TmLet Name Term Term
        | TmFix Term
        | TmFold Ty
        | TmUnfold Ty
        | TmProj Term Name
        | TmRecord [(Name, Term)]
        | TmTag Name [Term] Ty
        | TmCase Term [(Name, (Int, Term))]
        deriving (Eq, Show)

data Ty
        = TyVar Int Int
        | TyArr Ty Ty
        | TyAll Name Kind Ty
        | TyAbs Name Kind Ty
        | TyApp Ty Ty
        | TyRec Name Kind Ty
        | TyRecord [(Name, Ty)]
        | TyVariant [(Name, [Ty])]
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
        { moduleBind :: [(Name, Binding)]
        , moduleEval :: [Located Term]
        }
        deriving (Eq, Show)