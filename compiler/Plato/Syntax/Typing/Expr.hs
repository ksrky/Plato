{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

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

type family Clause (a :: TcFlag)
type instance Clause 'Untyped = ([LPat], LExpr 'Untyped)
type instance Clause 'Typed = ([LPat], Expr 'Typed)

type family Alt (a :: TcFlag)
type instance Alt 'Untyped = (LPat, LExpr 'Untyped)
type instance Alt 'Typed = (LPat, Expr 'Typed)

data Expr (a :: TcFlag) where
        VarE :: Ident -> Expr a
        AppE :: LExpr 'Untyped -> LExpr 'Untyped -> Expr 'Untyped
        AppE' :: Expr 'Typed -> Expr 'Typed -> Expr 'Typed
        AbsE :: Ident -> Maybe Type -> LExpr 'Untyped -> Expr 'Untyped
        AbsE' :: Ident -> Type -> Expr 'Typed -> Expr 'Typed
        TAppE :: Expr 'Typed -> [Type] -> Expr 'Typed
        TAbsE :: [Quant] -> Expr 'Typed -> Expr 'Typed
        LetE :: [((Ident, LType), [Clause 'Untyped])] -> LExpr 'Untyped -> Expr 'Untyped
        LetE' :: [((Ident, Type), Expr 'Typed)] -> LExpr 'Typed -> Expr 'Typed
        CaseE :: LExpr 'Untyped -> [Alt 'Untyped] -> Expr 'Untyped
        CaseE' :: Expr 'Typed -> Type -> [Alt 'Typed] -> Expr 'Typed
        AnnE :: LExpr 'Untyped -> Sigma -> Expr 'Untyped

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (Expr a)
deriving instance Show (Expr a)

instance HasLoc ([LPat], LExpr 'Untyped) where
        getLoc (pats, exp) = getLoc pats <> getLoc exp

instance HasLoc (LPat, LExpr 'Untyped) where
        getLoc (pat, exp) = getLoc pat <> getLoc exp

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
prClause :: Clause 'Untyped -> Doc ann
prClause (pats, exp) = hsep (map (pretty' 1) pats ++ [arrow, pretty exp])

instance Pretty (Expr a) where
        pretty = pretty' 0

instance PrettyWithContext (Expr a) where
        pretty' _ (VarE var) = prettyId var
        pretty' c (AppE fun arg) = contextParens c 0 $ hsep [pretty' 0 fun, pretty' 1 arg]
        pretty' c (AppE' fun arg) = contextParens c 0 $ hsep [pretty' 0 fun, pretty' 1 arg]
        pretty' c (AbsE var Nothing body) = contextParens c 0 $ hsep [backslash, pretty var, dot, pretty body]
        pretty' c (AbsE var (Just var_ty) body) =
                contextParens c 0 $ hsep [backslash, pretty var, colon, pretty var_ty, dot, pretty body]
        pretty' c (AbsE' var var_ty body) =
                contextParens c 0 $ hsep [backslash, pretty var, colon, pretty var_ty, dot, pretty body]
        pretty' c (TAppE fun []) = pretty' c fun
        pretty' c (TAppE fun tyargs) = contextParens c 0 $ hsep (pretty' 1 fun : map (pretty' 1) tyargs)
        pretty' c (TAbsE [] body) = pretty' c body
        pretty' c (TAbsE qnts body) = contextParens c 0 $ hsep [backslash, prQuants qnts, dot, pretty body]
        pretty' c (LetE decs body) =
                contextParens c 0 $ hsep ["let", braces $ map prdec decs `sepBy` semi, "in", pretty body]
            where
                prdec ((id, ty), clses) =
                        hsep [prettyId id, colon, pretty ty, "where", braces $ map prClause clses `sepBy` semi]
        pretty' c (LetE' decs body) =
                contextParens c 0 $ hsep ["let", braces $ map prdec decs `sepBy` semi, "in", pretty body]
            where
                prdec ((id, ty), exp) = hsep [prettyId id, colon, pretty ty, equals, pretty exp]
        pretty' c (CaseE match alts) =
                contextParens c 0 $
                        hsep
                                [ "case"
                                , pretty match
                                , "of"
                                , braces $ map (\(p, e) -> hsep [pretty p, arrow, pretty e]) alts `sepBy` semi
                                ]
        pretty' c (CaseE' match _ alts) =
                contextParens c 0 $
                        hsep
                                [ "case"
                                , pretty match
                                , "of"
                                , braces $ map (\(p, e) -> hsep [pretty p, arrow, pretty e]) alts `sepBy` semi
                                ]
        pretty' c (AnnE exp ty) = contextParens c 0 $ hsep [pretty exp, colon, pretty ty]