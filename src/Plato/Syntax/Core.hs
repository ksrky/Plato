module Plato.Syntax.Core where

import qualified Data.Vector as V

import Plato.Common.Name
import Plato.Core.Debug

data Term
        = TmVar Int Info
        | TmApp Term Term
        | TmAbs Info Type Term
        | TmTApp Term Type
        | TmTAbs Info Kind Term
        | TmLet Info Term Term
        | TmFix Term
        | TmPack
        | TmUnpack
        | TmProj Term Name
        | TmRecord [(Name, Term)]
        | TmTag Name [Term] Type
        | TmCase Term Type [(Name, Term)] Term
        deriving (Eq, Show)

data Type
        = TyVar Int Info
        | TyArr Type Type
        | TyAll Info Kind Type
        | TySome Info Kind Type
        | TyAbs Info Kind Type
        | TyApp Type Type
        | TyRecord [(Name, Type)]
        deriving (Eq, Show)

data Kind = KnStar | KnArr Kind Kind deriving (Eq, Show)

data Binding
        = NameBind
        | VarBind Type
        | TyVarBind Kind
        | TmAbbBind Term Type
        | TyAbbBind Type Kind

data Command
        = Bind Name Binding
        | SomeBind Name Name Term Type
        | Eval Term

data Module = Module
        { moduleBind :: [(Name, Binding)]
        , moduleEval :: [Term]
        }

type Context = V.Vector (Name, Binding)
