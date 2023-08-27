{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Syntax.Typing.Expr (
        LExpr,
        Clause,
        Expr (..),
        prClause,
) where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Syntax.Typing.Base (TcFlag (..))
import Plato.Syntax.Typing.Pat
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LExpr a = Located (Expr a)

type Clause a = ([LPat], LExpr a)

type Alt a = (LPat, LExpr a)

data Expr (a :: TcFlag) where
        VarE :: Ident -> Expr a
        AppE :: LExpr a -> LExpr a -> Expr a
        AbsE :: Ident -> Maybe Type -> LExpr 'Untyped -> Expr 'Untyped
        AbsEok :: Ident -> Type -> Expr 'Typed -> Expr 'Typed
        TAppE :: Expr 'Typed -> [Type] -> Expr 'Typed
        TAbsE :: [Quant] -> Expr 'Typed -> Expr 'Typed
        LetE :: [(Ident, [Clause 'Untyped])] -> [(Ident, LType)] -> LExpr 'Untyped -> Expr 'Untyped
        LetEok :: [(Ident, LExpr 'Typed)] -> [(Ident, LType)] -> LExpr 'Typed -> Expr 'Typed
        CaseE :: LExpr 'Untyped -> [Alt 'Untyped] -> Expr 'Untyped
        CaseEok :: LExpr 'Typed -> Type -> [Alt 'Typed] -> Expr 'Typed
        AnnE :: LExpr 'Untyped -> Sigma -> Expr 'Untyped

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (Expr a)
deriving instance Show (Expr a)

instance HasLoc (Clause a) where
        getLoc (pats, exp) = getLoc pats <> getLoc exp

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
prClause :: Clause a -> Doc ann
prClause (pats, exp) = hsep (map (pretty' 1) pats ++ [arrow, pretty exp])

prBinds :: [(Ident, [Clause 'Untyped])] -> Doc ann
prBinds bnds =
        map
                ( \(id, clses) ->
                        hsep
                                [ prettyId id
                                , "where"
                                , braces $ map prClause clses `sepBy` semi
                                ]
                )
                bnds
                `sepBy` semi

prBinds' :: [(Ident, LExpr 'Typed)] -> Doc ann
prBinds' bnds = map (\(id, exp) -> hsep [prettyId id, equals, pretty exp]) bnds `sepBy` semi

prSpecs :: [(Ident, LType)] -> Doc ann
prSpecs spcs = map (\(id, exp) -> hsep [prettyId id, equals, pretty exp]) spcs `sepBy` semi

instance Pretty (Expr a) where
        pretty = pretty' 0

instance PrettyWithContext (Expr a) where
        pretty' _ (VarE var) = prettyId var
        pretty' c (AppE fun arg) = contextParens c 0 $ hsep [pretty' 0 fun, pretty' 1 arg]
        pretty' c (AbsE var Nothing body) = contextParens c 0 $ hsep [backslash, pretty var, dot, pretty body]
        pretty' c (AbsE var (Just var_ty) body) =
                contextParens c 0 $ hsep [backslash, pretty var, colon, pretty var_ty, dot, pretty body]
        pretty' c (AbsEok var var_ty body) =
                contextParens c 0 $ hsep [backslash, pretty var, colon, pretty var_ty, dot, pretty body]
        pretty' c (TAppE fun []) = pretty' c fun
        pretty' c (TAppE fun tyargs) = contextParens c 0 $ hsep (pretty' 1 fun : map (pretty' 1) tyargs)
        pretty' c (TAbsE [] body) = pretty' c body
        pretty' c (TAbsE qnts body) = contextParens c 0 $ hsep [backslash, prQuants qnts, dot, pretty body]
        pretty' c (LetE bnds spcs body) =
                contextParens c 0 $ hsep ["let", braces $ prSpecs spcs <> semi <+> prBinds bnds, "in", pretty body]
        pretty' c (LetEok bnds spcs body) =
                contextParens c 0 $ hsep ["let", braces $ prSpecs spcs <> semi <+> prBinds' bnds, "in", pretty body]
        pretty' c (CaseE match alts) =
                contextParens c 0 $
                        hsep
                                [ "case"
                                , pretty match
                                , "of"
                                , braces $ map (\(p, e) -> hsep [pretty p, arrow, pretty e]) alts `sepBy` semi
                                ]
        pretty' c (CaseEok match _ alts) =
                contextParens c 0 $
                        hsep
                                [ "case"
                                , pretty match
                                , "of"
                                , braces $ map (\(p, e) -> hsep [pretty p, arrow, pretty e]) alts `sepBy` semi
                                ]
        pretty' c (AnnE exp ty) = contextParens c 0 $ hsep [pretty exp, colon, pretty ty]