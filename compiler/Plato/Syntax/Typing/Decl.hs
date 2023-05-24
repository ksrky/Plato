{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Syntax.Typing.Decl where

import Prettyprinter

import Plato.Common.Ident
import Plato.Syntax.Typing.Base
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------

data Bind (a :: TcFlag) where
        -- ValBind Ident LExpr |
        FunBind :: Ident -> [Clause 'TcUndone] -> Bind 'TcUndone
        FunBindok :: Ident -> LExpr 'TcDone -> Bind 'TcDone
        TypBind :: Ident -> LType -> Bind a
        DatBind :: Ident -> [Quant] -> [(Ident, LType)] -> Bind a

data Spec
        = ValSpec Ident LType
        | TypSpec Ident Kind
        deriving (Eq, Show)

data Decl a
        = BindDecl (Bind a)
        | SpecDecl Spec

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (Bind a)
deriving instance Show (Bind a)
deriving instance Eq (Decl a)
deriving instance Show (Decl a)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (Bind a) where
        pretty (FunBind id clauses) =
                hsep
                        [ pretty id
                        , equals
                        , "where"
                        , indent 4 (vsep (map prClause clauses))
                        ]
        pretty (FunBindok id exp) = hsep [pretty id, equals, pretty exp]
        pretty (TypBind id ty) = hsep [pretty id, equals, pretty ty]
        pretty (DatBind id params constrs) =
                hsep
                        [ "data"
                        , hsep (pretty id : [prQuants params])
                        , "where"
                        , vsep (map (\(con, ty) -> hsep [pretty con, colon, pretty ty]) constrs)
                        ]

instance Pretty Spec where
        pretty (ValSpec id ty) = hsep [pretty id, colon, pretty ty]
        pretty (TypSpec id kn) = hsep [pretty id, colon, pretty kn]

instance Pretty (Decl a) where
        pretty (BindDecl bnd) = pretty bnd
        pretty (SpecDecl spc) = pretty spc