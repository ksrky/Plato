module Plato.Syntax.Typing.Module where

import Prettyprinter

import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Ident
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Type
import Plato.Typing.Subst

newtype Sig
        = SigDecls [Decl]
        --  | SigFunctor Ident Sig Sig
        deriving (Eq, Show)

data Decl
        = ValueDecl Ident Type
        | TypeDecl Ident Kind
        | ModuleDecl Ident Sig
        deriving (Eq, Show)

data Mod
        = ModIdent Ident
        | ModBinds [Bind]
        | --  | ModFun Ident Sig Mod
          --  | ModApp Mod Mod
          ModConst Mod Sig
        deriving (Eq, Show)

data Bind
        = ValueBind Ident (Maybe Type) LExpr
        | TypeBind Ident (Maybe Kind) LType
        | ModuleBind Ident (Maybe Sig) Mod
        deriving (Eq, Show)

instance Substitutable Sig where
        subst sub (SigDecls decs) = SigDecls (map (subst sub) decs)

-- subst sub (SigFunctor id sig1 sig2) = SigFunctor id (subst sub sig1) (subst sub sig2)

instance Substitutable Decl where
        subst sub (ValueDecl id ty) = ValueDecl id (subst sub ty)
        subst sub (TypeDecl id kn) = TypeDecl id (subst sub kn)
        subst sub (ModuleDecl id mod) = ModuleDecl id (subst sub mod)

instance Pretty Sig where
        pretty (SigDecls decs) = vsep (map pretty decs)

-- pretty (SigFunctor id sig1 sig2) = hsep [parens (hsep [pretty id, colon, indent 4 (pretty sig1)]), "->", line, indent 4 (pretty sig2)]

instance Pretty Decl where
        pretty (ValueDecl id ty) = hsep [pretty id, colon, pretty ty]
        pretty (TypeDecl id kn) = hsep [pretty id, colon, pretty kn]
        pretty (ModuleDecl id sig) = hsep [pretty id, colon, line, indent 4 (pretty sig)]

instance Pretty Mod where
        pretty (ModIdent id) = pretty id
        pretty (ModBinds bnds) = vsep (map pretty bnds)
        -- pretty (ModFun id sig1 mod2) = hsep [parens (hsep [pretty id, colon, indent 4 (pretty sig1)]), "->", line, indent 4 (pretty mod2)]
        -- pretty (ModApp mod1 mod2) = hsep [parens (pretty mod1), parens (pretty mod2)]
        pretty (ModConst mod1 sig2) = hsep [pretty mod1, colon, pretty sig2]

instance Pretty Bind where
        pretty (ValueBind id _mty exp) = hsep [pretty id, equals, pretty exp]
        pretty (TypeBind id _mkn ty) = hsep [pretty id, equals, pretty ty]
        pretty (ModuleBind id _msig mod) = hsep [pretty id, equals, pretty mod]