{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Plato.Syntax.Typing.Expr (
        LExpr,
        XExpr,
        Clause,
        Alt,
        Clauses,
        Alts,
        Annot,
        Bind (..),
        XBind,
        XBinds,
        Expr (..),
        prClause,
) where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path
import Plato.Common.Pretty
import Plato.Syntax.Typing.Base
import Plato.Syntax.Typing.Pat
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LExpr a = Located (Expr a)

type family XExpr (a :: TcFlag) where
        XExpr 'Untyped = LExpr 'Untyped
        XExpr 'Typed = Expr 'Typed

type Clause (a :: TcFlag) = ([LPat], XExpr a)
type Alt (a :: TcFlag) = (LPat, XExpr a)

type Clauses (a :: TcFlag) = [Clause a]
type Alts (a :: TcFlag) = [Alt a]

type family Annot (a :: TcFlag) where
        Annot 'Untyped = Maybe LType
        Annot 'Typed = Type

data Bind (a :: TcFlag) = Bind (Ident, Annot a) (XExpr a)

type family XBind (a :: TcFlag) where
        XBind 'Untyped = Located (Bind 'Untyped)
        XBind 'Typed = Bind 'Typed

type XBinds (a :: TcFlag) = Block (XBind a)

data Expr (a :: TcFlag) where
        VarE :: Path -> Expr a
        AppE :: XExpr a -> XExpr a -> Expr a
        AbsE :: Ident -> Annot a -> XExpr a -> Expr a
        TAppE :: Expr 'Typed -> [Type] -> Expr 'Typed
        TAbsE :: Quants -> Expr 'Typed -> Expr 'Typed
        LetE :: XBinds a -> XExpr a -> Expr a
        CaseE :: XExpr a -> Annot a -> Alts a -> Expr a
        ClauseE :: Clauses 'Untyped -> Expr 'Untyped

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (Expr 'Untyped)
deriving instance Show (Expr 'Untyped)
deriving instance Eq (Expr 'Typed)
deriving instance Show (Expr 'Typed)
deriving instance Eq (Bind 'Untyped)
deriving instance Eq (Bind 'Typed)
deriving instance Show (Bind 'Untyped)
deriving instance Show (Bind 'Typed)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

prClause :: Clause 'Untyped -> Doc ann
prClause (pats, exp) = hsep (map (pretty' 1) pats ++ [arrow, pretty exp])

instance Pretty (Expr 'Untyped) where
        pretty' _ (VarE path) = pretty path
        pretty' p (AppE fun arg) = parenswPrec p 1 $ hsep [pretty' 1 fun, pretty' 2 arg]
        pretty' p (AbsE var Nothing body) = parenswPrec p 0 $ hsep [backslash, pretty var, dot, pretty body]
        pretty' p (AbsE var (Just var_ty) body) =
                parenswPrec p 0 $ hsep [backslash, pretty var, colon, pretty var_ty, dot, pretty body]
        pretty' p (LetE bnds body) =
                parenswPrec p 0 $ hsep ["let", braces $ pretty bnds, "in", pretty body]
        pretty' p (CaseE match _ alts) =
                parenswPrec p 0
                        $ hsep
                                [ "case"
                                , pretty match
                                , "of"
                                , braces $ map (\(p, e) -> hsep [pretty p, arrow, pretty e]) alts `sepBy` semi
                                ]
        pretty' _ (ClauseE clauses) =
                hsep [backslash, "where", encloseSep lbrace rbrace (semi <> space) (map prClause clauses)]

instance Pretty (Expr 'Typed) where
        pretty' _ (VarE var) = pretty var
        pretty' p (AppE fun arg) = parenswPrec p 1 $ hsep [pretty' 1 fun, pretty' 2 arg]
        pretty' p (AbsE var var_ty body) =
                parenswPrec p 0 $ hsep [backslash, pretty var, colon, pretty var_ty, dot, pretty body]
        pretty' p (TAppE fun []) = pretty' p fun
        pretty' p (TAppE fun tyargs) = parenswPrec p 0 $ hsep (pretty' 1 fun : map (pretty' 2) tyargs)
        pretty' p (TAbsE [] body) = pretty' p body
        pretty' p (TAbsE qnts body) = parenswPrec p 0 $ hsep [backslash, prQuants qnts, dot, pretty body]
        pretty' p (LetE bnds body) =
                parenswPrec p 0 $ hsep ["let", braces $ pretty bnds, "in", pretty body]
        pretty' p (CaseE match _ alts) =
                parenswPrec p 0
                        $ hsep
                                [ "case"
                                , pretty match
                                , "of"
                                , braces $ map (\(p, e) -> hsep [pretty p, arrow, pretty e]) alts `sepBy` semi
                                ]

instance Pretty (Bind 'Untyped) where
        pretty (Bind (id, Just ty) exp) = hsep [pretty id, colon, pretty ty, "where", pretty exp]
        pretty (Bind (id, Nothing) exp) = hsep [pretty id, "where", pretty exp]

instance Pretty (Bind 'Typed) where
        pretty (Bind (id, ty) exp) = hsep [pretty id, colon, pretty ty, "where", pretty exp]