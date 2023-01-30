module Plato.Syntax.Core where

import qualified Data.Vector as V
import Prettyprinter

import Plato.Common.Name
import Plato.Core.Debug

newtype Unique = Unique Word deriving (Eq, Show)

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
        | TmProj Term Unique Info
        | TmRecord [(Unique, (Info, Term))]
        | TmTag Info Unique [Term] Type
        | TmCase Term Type [(Unique, Term)]
        deriving (Eq, Show)

data Type
        = TyVar Int Info
        | TyArr Type Type
        | TyAll Info Kind Type
        | TyAbs Info Kind Type
        | TyApp Type Type
        | TyRec Info Kind Type
        | TyRecord [(Unique, (Info, Type))]
        | TyVariant [(Unique, (Info, [Type]))]
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

instance Pretty Unique where
        pretty (Unique i) = "$" <> viaShow i