{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Syntax.Typing.Expr (
        LExpr,
        Clause,
        Expr (..),
        prClause,
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
                                        , braces $ concatWith (surround $ semi <> space) (map prClause clses)
                                        ]
                        )
                        bnds
                )

prBinds' :: [(Ident, LExpr 'Typed)] -> Doc ann
prBinds' bnds =
        concatWith
                (surround $ semi <> space)
                (map (\(id, exp) -> hsep [pretty id, equals, pretty exp]) bnds)
prSpecs :: [(Ident, LType)] -> Doc ann
prSpecs spcs =
        concatWith
                (surround $ semi <> space)
                (map (\(id, exp) -> hsep [pretty id, equals, pretty exp]) spcs)

instance Pretty (Expr a) where
        pretty (VarE var) = pretty var
        pretty exp@AppE{} = prExpr2 exp
        pretty (AbsE var body) =
                hcat
                        [ backslash
                        , pretty var
                        , dot <+> pretty body
                        ]
        pretty (AbsEok var ann body) =
                hcat
                        [ backslash
                        , pretty var
                        , colon
                        , pretty ann
                        , dot <+> pretty body
                        ]
        pretty (TAppE fun tyargs) = hsep (prExpr1 fun : map pretty tyargs)
        pretty (TAbsE qnts body) =
                hcat [backslash, prQuants qnts, dot <+> pretty body]
        pretty (LetE bnds spcs body) =
                hsep
                        [ "let"
                        , braces $ prSpecs spcs <> semi <+> prBinds bnds
                        , "in"
                        , pretty body
                        ]
        pretty (LetEok bnds spcs body) =
                hsep
                        [ "let"
                        , braces $ prSpecs spcs <> semi <+> prBinds' bnds
                        , " in"
                        , pretty body
                        ]
        pretty (CaseE match alts) =
                hsep
                        [ "case"
                        , pretty match
                        , "of"
                        , braces $
                                concatWith
                                        (surround $ semi <> space)
                                        (map (\(p, e) -> hsep [pretty p, "->", pretty e]) alts)
                        ]
        pretty (CaseEok match _ alts) =
                hsep
                        [ "case"
                        , prExpr1 (unLoc match)
                        , "of"
                        , braces $
                                concatWith
                                        (surround $ semi <> space)
                                        (map (\(p, e) -> hsep [pretty p, "->", pretty e]) alts)
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