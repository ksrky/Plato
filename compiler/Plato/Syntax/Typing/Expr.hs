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

data Expr (a :: TcFlag) where
        VarE :: Ident -> Expr a
        AppE :: LExpr a -> LExpr a -> Expr a
        AbsE :: Ident -> LExpr 'Untyped -> Expr 'Untyped
        AbsEok :: Ident -> Type -> Expr 'Typed -> Expr 'Typed
        TAppE :: Expr 'Typed -> [Type] -> Expr 'Typed
        TAbsE :: [Quant] -> Expr 'Typed -> Expr 'Typed
        LetE :: [(Ident, [Clause 'Untyped])] -> [(Ident, LType)] -> LExpr 'Untyped -> Expr 'Untyped
        LetEok :: [(Ident, LExpr 'Typed)] -> [(Ident, LType)] -> LExpr 'Typed -> Expr 'Typed
        CaseE :: LExpr 'Untyped -> [(LPat, LExpr 'Untyped)] -> Expr 'Untyped
        CaseEok :: LExpr 'Typed -> Type -> [(LPat, LExpr 'Typed)] -> Expr 'Typed

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (Expr a)
deriving instance Show (Expr a)

instance HasLoc (Clause a) where
        getLoc (pats, exp) = combineSpans (getLoc pats) (getLoc exp)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
prClause :: Clause a -> Doc ann
prClause (pats, exp) = hsep (map prAtomPat pats ++ ["->", pretty exp])

prBinds :: [(Ident, [Clause 'Untyped])] -> Doc ann
prBinds bnds =
        concatWith
                (surround $ semi <> space)
                ( map
                        ( \(id, clses) ->
                                hsep
                                        [ pretty id
                                        , "where"
                                        , braces $ map prClause clses `sepBy` semi
                                        ]
                        )
                        bnds
                )

prBinds' :: [(Ident, LExpr 'Typed)] -> Doc ann
prBinds' bnds = map (\(id, exp) -> hsep [pretty id, equals, pretty exp]) bnds `sepBy` semi
prSpecs :: [(Ident, LType)] -> Doc ann
prSpecs spcs = map (\(id, exp) -> hsep [pretty id, equals, pretty exp]) spcs `sepBy` semi

instance Pretty (Expr a) where
        pretty (VarE var) = pretty var
        pretty exp@AppE{} = prExpr2 exp
        pretty (AbsE var body) = hsep [backslash, pretty var, dot, pretty body]
        pretty (AbsEok var ann body) =
                hsep [backslash, pretty var, colon, pretty ann, dot, pretty body]
        pretty (TAppE fun tyargs) = hsep (prExpr1 fun : map pretty tyargs)
        pretty (TAbsE qnts body) = hsep [backslash, prQuants qnts, dot, pretty body]
        pretty (LetE bnds spcs body) =
                hsep ["let", braces $ prSpecs spcs <> semi <+> prBinds bnds, "in", pretty body]
        pretty (LetEok bnds spcs body) =
                hsep ["let", braces $ prSpecs spcs <> semi <+> prBinds' bnds, "in", pretty body]
        pretty (CaseE match alts) =
                hsep
                        [ "case"
                        , pretty match
                        , "of"
                        , braces $ map (\(p, e) -> hsep [pretty p, "->", pretty e]) alts `sepBy` semi
                        ]
        pretty (CaseEok match _ alts) =
                hsep
                        [ "case"
                        , prExpr1 (unLoc match)
                        , "of"
                        , braces $ map (\(p, e) -> hsep [pretty p, "->", pretty e]) alts `sepBy` semi
                        ]

prExpr2 :: Expr a -> Doc ann
prExpr2 e = walk e []
    where
        walk :: Expr a -> [Expr a] -> Doc ann
        walk (AppE fun arg) acc = walk (unLoc fun) (unLoc arg : acc)
        walk fun args = prExpr1 fun <+> hsep (map prExpr1 args)

prExpr1 :: Expr a -> Doc ann
prExpr1 e@VarE{} = pretty e
prExpr1 e = parens (pretty e)