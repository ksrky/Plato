module Plato.Syntax.Core where

import qualified Data.Vector as V

import Plato.Common.Name
import Plato.Core.Debug

data Term
        = TmVar Info Int
        | TmApp Term Term
        | TmAbs Info Type Term
        | TmTApp Term Type
        | TmTAbs Info Kind Term
        | TmFix Term
        | TmFold Type
        | TmUnfold Type
        | TmProj Term Name
        | TmRecord [(Name, Term)]
        | TmTag Name Term Type
        | TmCase Term [(Term -> Maybe [Term], Term)]

data Type
        = TyVar Info Int
        | TyArr Type Type
        | TyAll Info Kind Type
        | TyAbs Info Kind Type
        | TyApp Type Type
        | TyRec Info Kind Type
        | TyRecord [(Name, Type)]
        | TyVariant [(Name, Type)]
        deriving (Eq, Show)

data Kind = KnStar | KnArr Kind Kind deriving (Eq, Show)

data Binding
        = NameBind
        | VarBind Type
        | TyVarBind Kind
        | TmAbbBind Term Type
        | TyAbbBind Type Kind

data Module = Module
        { moduleName :: ModuleName
        , moduleBind :: [(Name, Binding)]
        , moduleEval :: [Term]
        }

type Context = V.Vector (Name, Binding)