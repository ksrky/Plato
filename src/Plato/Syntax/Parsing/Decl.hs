{-# LANGUAGE TupleSections #-}

module Plato.Syntax.Parsing.Decl where

import Prettyprinter

import Plato.Common.Fixity
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path
import Plato.Syntax.Parsing.Expr
import Plato.Syntax.Parsing.Pat
import Plato.Syntax.Parsing.Type

type LDecl = Located Decl
type LModule = Located Module

data Decl
        = OpenD Path
        | FixityD Fixity Ident
        | ModuleD Ident LModule
        | DataD Ident [Ident] [(Ident, LType)]
        | FuncSigD Ident LType
        | FuncD Ident [LPat] LExpr
        deriving (Eq, Show)

newtype Module = Module [LDecl] deriving (Eq, Show)

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
instance Substitutable Decl where
        substPath (OpenD path) = OpenD <$> substPath path
        substPath dec@FixityD{} = return dec
        substPath (ModuleD id mod) = ModuleD id <$> substPath `traverse` mod
        substPath (DataD id params fields) =
                DataD id params
                        <$> mapM (\(con, ty) -> (con,) <$> substPath `traverse` ty) fields
        substPath (FuncSigD id ty) = FuncSigD id <$> substPath `traverse` ty
        substPath (FuncD id params body) = FuncD id <$> mapM (substPath `traverse`) params <*> substPath `traverse` body

instance Substitutable Module where
        substPath (Module decs) = Module <$> mapM (substPath `traverse`) decs

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Decl where
        pretty (OpenD path) = hsep ["open", pretty path]
        pretty (FixityD (Fixity prec dir) op) =
                hsep
                        [ pretty dir
                        , pretty prec
                        , pretty op
                        ]
        pretty (ModuleD id mod) = hsep ["module", pretty id, "where", indent 4 (pretty mod)]
        pretty (DataD id args fields) =
                hsep
                        [ "data"
                        , hsep (pretty id : map pretty args)
                        , "where"
                        , vcat (map (pretty . uncurry FuncSigD) fields)
                        ]
        pretty (FuncSigD var body_ty) = hsep [pretty var, colon, pretty body_ty]
        pretty (FuncD var args body) =
                hsep
                        [ hsep (pretty var : map pretty args)
                        , equals
                        , pretty body
                        ]

instance Pretty Module where
        pretty (Module decs) = vcat (map pretty decs)