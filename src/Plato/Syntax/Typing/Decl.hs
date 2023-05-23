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

-- \| TypeBind Ident (Maybe Kind) LType

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
instance Eq (Bind a)
instance Show (Bind a)
instance Eq (Decl a)
instance Show (Decl a)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (Bind a)
instance Pretty Spec
instance Pretty (Decl a)