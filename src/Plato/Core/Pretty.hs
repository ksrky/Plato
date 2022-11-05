{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Plato.Core.Pretty where

import Plato.Common.GlbName
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Syntax.Core
import Prettyprinter

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
hsep' :: [Doc ann] -> Doc ann
hsep' docs = if null docs then emptyDoc else emptyDoc <+> hsep docs

class PrettyCore a where
        ppr :: Context -> a -> Doc ann

instance PrettyCore Term where
        ppr ctx t = case t of
                TmVar x n -> if length ctx == n then pretty $ index2name ctx x else "[bad index]"
                TmAbs x tyT1 t2 ->
                        let (x', ctx') = pickFreshName x ctx
                         in "\\" <> pretty x' <> colon <> ppr ctx tyT1 <> dot <+> ppr ctx' t2
                TmApp{} -> pprapp ctx t
                TmTApp t1 tyT2 -> ppr ctx t1 <+> ppr ctx tyT2
                TmTAbs tyX t2 ->
                        let (tyX', ctx') = pickFreshName tyX ctx
                         in "\\" <> pretty tyX' <> dot <+> ppr ctx' t2
                TmLet x t1 t2 ->
                        let (x', ctx') = pickFreshName x ctx
                         in hsep ["let", pretty x', equals, ppr ctx t1, "in", ppr ctx' t2]
                TmFix t1 -> "fix" <+> ppr ctx t1
                TmFold tyT -> "fold" <+> lbracket <> ppr ctx tyT <> rbracket
                TmUnfold tyT -> "unfold" <+> lbracket <> ppr ctx tyT <> rbracket
                TmProj t1 l -> ppr ctx t1 <> dot <> pretty l
                TmRecord fields ->
                        if null fields
                                then lbrace <> rbrace
                                else
                                        hsep
                                                [ lbrace
                                                , concatWith (\d -> (<+> comma <+> d)) (map (\(xi, ti) -> pretty xi <+> equals <+> ppr ctx ti) fields)
                                                , rbrace
                                                ]
                TmTag li ts1 _ -> hcat [pretty li, hsep' (map (pprtm ctx) ts1)]
                TmCase t1 alts ->
                        "case" <+> ppr ctx t1 <+> "of" <+> lbrace <> line
                                <> indent
                                        4
                                        ( vsep
                                                ( map
                                                        ( \(li, (ki, ti)) ->
                                                                let ctx' = foldr addFreshName ctx (map (newName . str2varName . show) [1 .. ki])
                                                                 in hsep [pretty li, pretty ki, "->", ppr ctx' ti]
                                                        )
                                                        alts
                                                )
                                        )
                                <> line
                                <> rbrace

pprtm :: Context -> Term -> Doc ann
pprtm ctx t@TmVar{} = ppr ctx t
pprtm ctx t@TmFold{} = ppr ctx t
pprtm ctx t@TmUnfold{} = ppr ctx t
pprtm ctx t@(TmTag _ as _) | null as = ppr ctx t
pprtm ctx t = parens $ ppr ctx t

pprapp :: Context -> Term -> Doc ann
pprapp ctx t = walk t []
    where
        walk :: Term -> [Term] -> Doc ann
        walk (TmApp t1 t2) ts = walk t1 (t2 : ts)
        walk t' ts = pprtm ctx t' <+> sep (map (pprtm ctx) ts)

instance PrettyCore Ty where
        ppr ctx t = case t of
                TyVar x n -> if length ctx == n then pretty $ index2name ctx x else "[bad index]"
                TyArr tyT1 tyT2 -> pprty ArrPrec ctx tyT1 <+> "->" <+> pprty TopPrec ctx tyT2
                TyAll tyX knK1 tyT2 ->
                        let (tyX', ctx') = pickFreshName tyX ctx
                         in hcat [lbrace, pretty tyX', colon, ppr ctx knK1, rbrace, dot <+> ppr ctx' tyT2]
                TyApp tyT1 tyT2 -> ppr ctx tyT1 <+> pprty AppPrec ctx tyT2
                TyAbs tyX knK1 tyT2 ->
                        let (tyX', ctx') = pickFreshName tyX ctx
                         in hcat [backslash, pretty tyX', colon, ppr ctx knK1, dot <+> ppr ctx' tyT2]
                TyRec tyX knK1 tyT2 ->
                        let (tyX', ctx') = pickFreshName tyX ctx
                         in hcat [backslash, pretty tyX', colon, ppr ctx knK1, dot <+> ppr ctx' tyT2]
                TyRecord fields ->
                        if null fields
                                then lbrace <> rbrace
                                else
                                        hsep
                                                [ lbrace
                                                , concatWith (\d -> (<+> comma <+> d)) (map (\(xi, tyTi) -> pretty xi <+> colon <+> ppr ctx tyTi) fields)
                                                , rbrace
                                                ]
                TyVariant fields ->
                        langle
                                <> concatWith
                                        (\d e -> d <+> pipe <+> e)
                                        ( map
                                                (\(c, tys) -> pretty c <> hsep' (map (pprty AppPrec ctx) tys))
                                                fields
                                        )
                                <> rangle

data Prec = TopPrec | ArrPrec | AppPrec | AtomPrec deriving (Enum)

precty :: Ty -> Prec
precty TyAll{} = TopPrec
precty TyArr{} = ArrPrec
precty _ = AtomPrec

pprty :: Prec -> Context -> Ty -> Doc ann
pprty p ctx ty
        | fromEnum p >= fromEnum (precty ty) = parens (ppr ctx ty)
        | otherwise = ppr ctx ty

instance PrettyCore Kind where
        ppr _ KnStar = "*"
        ppr ctx (KnArr knK1 knK2) = pprkn ctx knK1 <+> ppr ctx knK2

pprkn :: Context -> Kind -> Doc ann
pprkn _ KnStar = "*"
pprkn ctx knK = parens (ppr ctx knK)

instance PrettyCore Binding where
        ppr _ NameBind = emptyDoc
        ppr ctx (VarBind tyT) = ppr ctx (unLoc tyT)
        ppr ctx (TyVarBind knK) = ppr ctx knK
        ppr ctx (TmAbbBind t tyT) = hsep [ppr ctx (unLoc t), colon, ppr ctx (unLoc tyT)]
        ppr ctx (TyAbbBind tyT knK) = hsep [ppr ctx (unLoc tyT), colon, ppr ctx knK]

instance PrettyCore Command where
        ppr _ (Import imp) = pretty imp
        ppr ctx (Bind x bind) = hsep [pretty x, equals, ppr ctx bind]
        ppr ctx (Eval t) = ppr ctx (unLoc t)

instance PrettyCore [Command] where
        ppr _ [] = emptyDoc
        ppr ctx (cmd@(Bind x _) : cmds) =
                let ctx' = addFreshName x ctx
                 in vsep [ppr ctx cmd, ppr ctx' cmds]
        ppr ctx (cmd : cmds) = vsep [ppr ctx cmd, ppr ctx cmds]
