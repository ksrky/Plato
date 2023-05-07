{-# LANGUAGE TupleSections #-}

module Plato.Syntax.Typing.Module where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LModule = Located Module

data Bind
        = ValueBind Ident (Maybe Type) LExpr
        | -- | TypeBind Ident (Maybe Kind) LType
          DataBind Ident [(Ident, LType)]
        | ModuleBind Ident LModule
        deriving (Eq, Show)

data Spec
        = ValueSpec Ident LType
        | TypeSpec Ident Kind
        deriving (Eq, Show)

data Decl
        = OpenDecl Path
        | FixityDecl
        | BindDecl Bind
        | SpecDecl Spec
        deriving (Eq, Show)

newtype Module = Module [Decl] deriving (Eq, Show)

newtype Signature = Signature [Spec] deriving (Eq, Show)

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
instance Substitutable Bind where
        substPath (ValueBind id mty exp) =
                ValueBind id <$> substPath `traverse` mty <*> substPath `traverse` exp
        -- substPath (TypeBind id mkn ty) = TypeBind id <$> substPath `traverse` mkn <*> substPath `traverse` ty
        substPath (DataBind id fields) =
                DataBind id <$> mapM (\(con, ty) -> (con,) <$> substPath `traverse` ty) fields
        substPath (ModuleBind id mod) = ModuleBind id <$> substPath `traverse` mod

instance Substitutable Spec where
        substPath (ValueSpec id ty) = ValueSpec id <$> substPath `traverse` ty
        substPath (TypeSpec id kn) = TypeSpec id <$> substPath kn

instance Substitutable Decl where
        substPath (OpenDecl path) = OpenDecl <$> substPath path
        substPath dec@FixityDecl{} = return dec
        substPath (BindDecl bnd) = BindDecl <$> substPath bnd
        substPath (SpecDecl spc) = SpecDecl <$> substPath spc

instance Substitutable Module where
        substPath (Module decs) = Module <$> mapM substPath decs

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Bind where
        pretty (ValueBind id _mty exp) = hsep [pretty id, equals, pretty exp]
        -- pretty (TypeBind id_mkn ty) = hsep [pretty id, equals, pretty ty]
        pretty (DataBind id fields) =
                hsep ["data", pretty id, "where", line, indent 4 (vsep $ map pretty fields)]
        pretty (ModuleBind id mod) = hsep [pretty id, equals, pretty mod]

instance Pretty Spec where
        pretty (ValueSpec id ty) = hsep [pretty id, colon, pretty ty]
        pretty (TypeSpec id kn) = hsep [pretty id, colon, pretty kn]

instance Pretty Decl where
        pretty (OpenDecl path) = hsep ["open", pretty path]
        pretty FixityDecl = "infix" -- tmp
        pretty (BindDecl bnd) = pretty bnd
        pretty (SpecDecl spc) = pretty spc

instance Pretty Module where
        pretty (Module decs) = vsep (map pretty decs)
