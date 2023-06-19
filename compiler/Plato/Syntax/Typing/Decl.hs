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
data Defn (a :: TcFlag) where
        -- ValDefn Ident LExpr |
        FunDefn :: Ident -> [Clause 'TcUndone] -> Defn 'TcUndone
        FunDefnok :: Ident -> LExpr 'TcDone -> Defn 'TcDone
        TypDefn :: Ident -> LType -> Defn a
        DatDefn :: Ident -> [Quant] -> [(Ident, LType)] -> Defn 'TcUndone
        DatDefnok :: Ident -> Kind -> [Quant] -> [(Ident, LType)] -> Defn 'TcDone

data Spec
        = ValSpec Ident LType
        | TypSpec Ident Kind
        deriving (Eq, Show)

data Decl a
        = DefnDecl (Defn a)
        | SpecDecl Spec

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (Defn a)
deriving instance Show (Defn a)
deriving instance Eq (Decl a)
deriving instance Show (Decl a)
instance Numbered (Decl a) where
        toNumber (SpecDecl TypSpec{}) = 0
        toNumber (DefnDecl DatDefn{}) = 1
        toNumber (DefnDecl DatDefnok{}) = 1
        toNumber (DefnDecl TypDefn{}) = 2
        toNumber (SpecDecl ValSpec{}) = 3
        toNumber (DefnDecl FunDefn{}) = 4
        toNumber (DefnDecl FunDefnok{}) = 4

instance Ord (Decl a) where
        compare x y = compare (toNumber x) (toNumber y)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (Defn a) where
        pretty (FunDefn id clauses) =
                hsep
                        [ pretty id
                        , "where"
                        , braces $ concatWith (surround $ semi <> space) (map prClause clauses)
                        ]
        pretty (FunDefnok id exp) = hsep [pretty id, equals, pretty exp]
        pretty (TypDefn id ty) = hsep [pretty id, equals, pretty ty]
        pretty (DatDefn id params constrs) =
                hsep
                        [ "data"
                        , hsep (pretty id : map (parens . prQuant) params)
                        , "where"
                        , braces $
                                concatWith
                                        (surround (semi <> space))
                                        (map (\(con, ty) -> hsep [pretty con, colon, pretty ty]) constrs)
                        ]
        pretty (DatDefnok id _ params constrs) = pretty (DatDefn id params constrs)

instance Pretty Spec where
        pretty (ValSpec id ty) = hsep [pretty id, colon, pretty ty]
        pretty (TypSpec id kn) = hsep [pretty id, colon, pretty kn]

instance Pretty (Decl a) where
        pretty (DefnDecl def) = pretty def
        pretty (SpecDecl spc) = pretty spc