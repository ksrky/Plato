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
                        , colon <+> pretty ann
                        , dot <+> pretty body
                        ]
        pretty (TAppE fun tyargs) = hsep (pretty fun : map pretty tyargs)
        pretty (TAbsE qnts body) =
                hcat
                        [ backslash
                        , prQuants qnts
                        , dot <+> pretty body
                        ]
        pretty (LetE bnds spcs body) =
                hsep
                        [ "let"
                        , lbrace <> line
                        , indent 4 (vsep (map pretty spcs ++ map pretty bnds)) <> line
                        , rbrace
                        , "in"
                        , pretty body
                        ]
        pretty (LetEok bnds spcs body) =
                hsep
                        [ "let"
                        , lbrace <> line
                        , indent 4 (vsep (map pretty spcs ++ map pretty bnds)) <> line
                        , rbrace
                        , " in"
                        , pretty body
                        ]
        pretty (CaseE match alts) =
                hsep
                        [ "case"
                        , pretty match
                        , "of"
                        , lbrace <> line
                        , indent 4 (vsep (map (\(p, e) -> hsep [pretty p, "->", pretty e]) alts))
                                <> line
                        , rbrace
                        ]

prExpr2 :: Expr a -> Doc ann
prExpr2 e = walk e []
    where
        walk :: Expr a -> [Expr a] -> Doc ann
        walk (AppE fun arg) acc = walk (unLoc fun) (unLoc arg : acc)
        walk fun args = prExpr1 fun <+> sep (map prExpr1 args)

prExpr1 :: Expr a -> Doc ann
prExpr1 e@VarE{} = pretty e
prExpr1 e = parens (pretty e)