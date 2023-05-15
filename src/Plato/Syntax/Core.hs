module Plato.Syntax.Core where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name

data Info = Info {info_loc :: Span, actual_name :: Name} deriving (Eq, Show)

mkInfo :: Ident -> Info
mkInfo id = Info (getLoc id) (nameIdent id)

data Term
        = TmVar !Int Info
        | TmApp Term Term
        | TmAbs Info Type Term
        | TmTApp Term Type
        | TmTAbs Info Kind Term
        | TmLet Info Term Term
        | TmFix Term
        | TmProj Term Name
        | TmRecord [(Name, Term)]
        | TmCon Label [Term]
        deriving (Eq, Show)

data Type
        = TyVar !Int Info
        | TyFun Type Type
        | TyAll Info Kind Type
        | TyApp Type Type
        | TyRecord [(Name, Type)]
        deriving (Eq, Show)

data Kind = KnStar | KnFun Kind Kind
        deriving (Eq, Show)

data Binding
        = NameBind
        | VarBind Type
        | TyVarBind Kind
        | TmAbbBind Term Type
        | TyAbbBind Type Kind

data Command
        = Bind Name Binding
        | Eval Term