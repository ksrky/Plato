module Plato.Syntax.Core where

import qualified Data.Vector as V
import Prettyprinter

import Plato.Common.Name
import Plato.Core.Debug

newtype Id = Id Word deriving (Eq, Show)

data Term
        = TmVar Int Info
        | TmApp Term Term
        | TmAbs Info Type Term
        | TmTApp Term Type
        | TmTAbs Info Kind Term
        | TmLet Info Term Term
        | TmFix Term
        | TmFold Type
        | TmUnfold Type
        | TmProj Term Id
        | TmRecord [(Id, Term)]
        | TmTag Id [Term] Type
        | TmCase Term Type [(Id, Term)] Term
        deriving (Eq, Show)

data Type
        = TyVar Int Info
        | TyArr Type Type
        | TyAll Info Kind Type
        | TyAbs Info Kind Type
        | TyApp Type Type
        | TyRec Info Kind Type
        | TyRecord [(Id, Type)]
        | TyVariant [(Id, [Type])]
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

instance Pretty Id where
        pretty (Id i) = "$" <> viaShow i