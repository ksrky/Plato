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
        AbsE :: Ident -> LExpr 'TcUndone -> Expr 'TcUndone
        AbsEok :: Ident -> Type -> Expr 'TcDone -> Expr 'TcDone
        TAppE :: Expr 'TcDone -> [Type] -> Expr 'TcDone
        TAbsE :: [Quant] -> Expr 'TcDone -> Expr 'TcDone
        LetE :: [(Ident, [Clause 'TcUndone])] -> [(Ident, LType)] -> LExpr 'TcUndone -> Expr 'TcUndone
        LetEok :: [(Ident, LExpr 'TcDone)] -> [(Ident, LType)] -> LExpr 'TcDone -> Expr 'TcDone
        CaseE :: LExpr a -> [(LPat, LExpr a)] -> Expr a

deriving instance Eq (Expr a)
deriving instance Show (Expr a)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
prClause :: Clause a -> Doc ann
prClause (pats, exp) = hsep (map prAtomPat pats ++ ["->", pretty exp])

prBinds :: [(Ident, [Clause 'TcUndone])] -> Doc ann
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

prBinds' :: [(Ident, LExpr 'TcDone)] -> Doc ann
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

prExpr2 :: Expr a -> Doc ann
prExpr2 e = walk e []
    where
        walk :: Expr a -> [Expr a] -> Doc ann
        walk (AppE fun arg) acc = walk (unLoc fun) (unLoc arg : acc)
        walk fun args = prExpr1 fun <+> hsep (map prExpr1 args)

prExpr1 :: Expr a -> Doc ann
prExpr1 e@VarE{} = pretty e
prExpr1 e = parens (pretty e)