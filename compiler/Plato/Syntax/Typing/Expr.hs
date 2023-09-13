{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Plato.Syntax.Typing.Expr (
        LExpr,
        Clause,
        Clauses,
        Expr (..),
        prClause,
) where

import Data.Graph

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Syntax.Typing.Base (TcFlag (..))
import {-# SOURCE #-} Plato.Syntax.Typing.Decl
import Plato.Syntax.Typing.Pat
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LExpr a = Located (Expr a)

type family Clause (a :: TcFlag)
type instance Clause 'Untyped = ([LPat], LExpr 'Untyped)
type instance Clause 'Typed = ([LPat], Expr 'Typed)

type family Alt (a :: TcFlag)
type instance Alt 'Untyped = (LPat, LExpr 'Untyped)
type instance Alt 'Typed = (LPat, Expr 'Typed)

type Clauses (a :: TcFlag) = [Clause a]
type Alts (a :: TcFlag) = [Alt a]

data Expr (a :: TcFlag) where
        VarE :: Ident -> Expr a
        AppE :: LExpr 'Untyped -> LExpr 'Untyped -> Expr 'Untyped
        AppE' :: Expr 'Typed -> Expr 'Typed -> Expr 'Typed
        AbsE :: Ident -> Maybe Type -> LExpr 'Untyped -> Expr 'Untyped
        AbsE' :: Ident -> Type -> Expr 'Typed -> Expr 'Typed
        TAppE :: Expr 'Typed -> [Type] -> Expr 'Typed
        TAbsE :: Quants -> Expr 'Typed -> Expr 'Typed
        LetE :: (SCC (Bind 'Untyped)) -> LExpr 'Untyped -> Expr 'Untyped
        LetE' :: (SCC (Bind 'Typed)) -> LExpr 'Typed -> Expr 'Typed
        CaseE :: LExpr 'Untyped -> Alts 'Untyped -> Expr 'Untyped
        CaseE' :: Expr 'Typed -> Type -> Alts 'Typed -> Expr 'Typed
        ClauseE :: Clauses 'Untyped -> Expr 'Untyped

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (Expr a)
deriving instance Show (Expr a)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
prClause :: Clause 'Untyped -> Doc ann
prClause (pats, exp) = hsep (map (pretty' 1) pats ++ [arrow, pretty exp])

instance Pretty (Expr a) where
        pretty = pretty' 0

instance PrettyWithContext (Expr a) where
        pretty' _ (VarE var) = pretty var
        pretty' p (AppE fun arg) = parenswPrec p 1 $ hsep [pretty' 1 fun, pretty' 2 arg]
        pretty' p (AppE' fun arg) = parenswPrec p 1 $ hsep [pretty' 1 fun, pretty' 2 arg]
        pretty' p (AbsE var Nothing body) = parenswPrec p 0 $ hsep [backslash, pretty var, dot, pretty body]
        pretty' p (AbsE var (Just var_ty) body) =
                parenswPrec p 0 $ hsep [backslash, pretty var, colon, pretty var_ty, dot, pretty body]
        pretty' p (AbsE' var var_ty body) =
                parenswPrec p 0 $ hsep [backslash, pretty var, colon, pretty var_ty, dot, pretty body]
        pretty' p (TAppE fun []) = pretty' p fun
        pretty' p (TAppE fun tyargs) = parenswPrec p 0 $ hsep (pretty' 1 fun : map (pretty' 2) tyargs)
        pretty' p (TAbsE [] body) = pretty' p body
        pretty' p (TAbsE qnts body) = parenswPrec p 0 $ hsep [backslash, prQuants qnts, dot, pretty body]
        pretty' p (LetE bnds body) =
                parenswPrec p 0 $ hsep ["let", braces $ pretty bnds, "in", pretty body]
        pretty' p (LetE' bnds body) =
                parenswPrec p 0 $ hsep ["let", braces $ pretty bnds, "in", pretty body]
        pretty' p (CaseE match alts) =
                parenswPrec p 0 $
                        hsep
                                [ "case"
                                , pretty match
                                , "of"
                                , braces $ map (\(p, e) -> hsep [pretty p, arrow, pretty e]) alts `sepBy` semi
                                ]
        pretty' p (CaseE' match _ alts) =
                parenswPrec p 0 $
                        hsep
                                [ "case"
                                , pretty match
                                , "of"
                                , braces $ map (\(p, e) -> hsep [pretty p, arrow, pretty e]) alts `sepBy` semi
                                ]
        pretty' _ (ClauseE clauses) =
                hsep [backslash, "where", encloseSep lbrace rbrace (semi <> space) (map prClause clauses)]