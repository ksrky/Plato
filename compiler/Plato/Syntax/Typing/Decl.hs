{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Syntax.Typing.Decl where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Utils
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
        DatBind :: Ident -> [Quant] -> [(Ident, LType)] -> Bind 'TcUndone
        DatBindok :: Ident -> Kind -> [Quant] -> [(Ident, LType)] -> Bind 'TcDone

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
instance Numbered (Decl a) where
        toNumber (SpecDecl TypSpec{}) = 0
        toNumber (SpecDecl ValSpec{}) = 1
        toNumber (BindDecl DatBind{}) = 2
        toNumber (BindDecl DatBindok{}) = 2
        toNumber (BindDecl TypBind{}) = 3
        toNumber (BindDecl FunBind{}) = 4
        toNumber (BindDecl FunBindok{}) = 4

instance Ord (Decl a) where
        compare x y = compare (toNumber x) (toNumber y)

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
                        , pretty id
                        , hsep (pretty id : map (parens . prQuant) params)
                        , "where"
                        , braces $
                                concatWith
                                        (surround (semi <> space))
                                        (map (\(con, ty) -> hsep [pretty con, colon, pretty ty]) constrs)
                        ]
        pretty (DatBindok id _ params constrs) = pretty (DatBind id params constrs)

instance Pretty Spec where
        pretty (ValSpec id ty) = hsep [pretty id, colon, pretty ty]
        pretty (TypSpec id kn) = hsep [pretty id, colon, pretty kn]

instance Pretty (Decl a) where
        pretty (BindDecl bnd) = pretty bnd
        pretty (SpecDecl spc) = pretty spc