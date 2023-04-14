{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Plato.Syntax.Abstract.Module where

import Prettyprinter

import qualified Data.Kind
import Plato.Common.Ident
import Plato.Common.Path
import Plato.Syntax.Abstract.Expr
import Plato.Syntax.Abstract.Kind
import Plato.Syntax.Abstract.Type

----------------------------------------------------------------
-- Datas
----------------------------------------------------------------
data Bind :: Data.Kind.Type -> Data.Kind.Type where
        ValueBind :: Ident -> Maybe (Type a) -> LExpr a -> Bind a
        TypeBind :: Ident -> Maybe (Kind a) -> LType a -> Bind a
        ModuleBind :: Ident -> Module a -> Bind a

data Spec :: Data.Kind.Type -> Data.Kind.Type where
        ValueSpec :: Ident -> Type a -> Spec a
        TypeSpec :: Ident -> Kind a -> Spec a

data Decl :: Data.Kind.Type -> Data.Kind.Type where
        OpenDecl :: Decl a
        FixityDecl :: Decl a
        BindDecl :: Bind a -> Decl a
        SpecDecl :: Spec a -> Decl a

newtype Module :: Data.Kind.Type -> Data.Kind.Type where
        Module :: [Decl a] -> Module a

newtype Signature :: Data.Kind.Type -> Data.Kind.Type where
        Signature :: [Spec a] -> Signature a

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
instance Substitutable (Spec a) where
        subst sub (ValueSpec id ty) = ValueSpec id (subst sub ty)
        subst sub (TypeSpec id kn) = TypeSpec id (subst sub kn)

instance Eq (Decl a)
instance Show (Decl a)
instance Pretty (Decl a)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
{-}
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
        pretty (ModuleBind id _msig mod) = hsep [pretty id, equals, pretty mod]-}