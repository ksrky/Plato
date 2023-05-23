{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Syntax.Typing.Expr (
        LExpr,
        Clause,
        Expr (..),
) where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Typing.Base (TcFlag (..))
import Plato.Syntax.Typing.Pat
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LExpr a = Located (Expr a)

type Clause a = ([LPat], LExpr a)

data Expr (a :: TcFlag) where
        VarE :: Ident -> Expr a
        AppE :: LExpr a -> LExpr a -> Expr a
        AbsE :: Ident -> LExpr 'TcUndone -> Expr 'TcUndone
        AbsEok :: Ident -> Type -> LExpr 'TcDone -> Expr 'TcDone
        TAppE :: LExpr 'TcDone -> [Type] -> Expr 'TcDone
        TAbsE :: [Quant] -> LExpr 'TcDone -> Expr 'TcDone
        LetE :: [(Ident, [Clause 'TcUndone])] -> [(Ident, LType)] -> LExpr 'TcUndone -> Expr 'TcUndone
        LetEok :: [(Ident, LExpr 'TcDone)] -> [(Ident, LType)] -> LExpr 'TcDone -> Expr 'TcDone
        CaseE :: LExpr a -> [(LPat, LExpr a)] -> Expr a

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
instance Eq (Expr a)
instance Show (Expr a)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (Expr a)