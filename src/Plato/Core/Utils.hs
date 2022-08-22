{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Plato.Core.Utils where

import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Core.Context
import Plato.Core.Syntax

import Control.Monad.State
import Data.List
import Plato.Core.Command

----------------------------------------------------------------
-- Pretty
----------------------------------------------------------------
-- todo: outer paren
-- pretty ctx b t = outer b $ case t of ...
-- outer True str = "(" ++ str ++ ")"
-- outer False str = str
instance Pretty (Context, Term) where
        pretty (ctx, t) = case t of
                TmVar _ x n -> if length ctx == n then pretty $ index2name ctx x else "[bad index]"
                TmAbs _ x tyT1 t2 ->
                        let (x', ctx') = pickFreshName x ctx
                         in "(\\" ++ pretty x' ++ ": " ++ pretty (ctx, tyT1) ++ ". " ++ pretty (ctx', t2) ++ ")"
                TmApp _ t1 t2 -> "(" ++ pretty (ctx, t1) ++ " " ++ pretty (ctx, t2) ++ ")"
                TmTAbs _ tyX knK1 t2 ->
                        let (tyX', ctx') = pickFreshName tyX ctx
                         in "(\\" ++ pretty tyX' ++ ": " ++ pretty (ctx, knK1) ++ ". " ++ pretty (ctx', t2) ++ ")"
                TmTApp _ t1 tyT2 -> "(" ++ pretty (ctx, t1) ++ " [" ++ pretty (ctx, tyT2) ++ "]" ++ ")"
                TmLet _ x t1 t2 ->
                        let (x', ctx') = pickFreshName x ctx
                         in "(let {" ++ pretty x' ++ "=" ++ pretty (ctx, t1) ++ "} in " ++ pretty (ctx', t2) ++ ")"
                TmFix _ t1 -> "(fix " ++ pretty (ctx, t1) ++ ")"
                TmFold _ tyT -> "(fold [" ++ pretty (ctx, tyT) ++ "])"
                TmUnfold _ tyT -> "(unfold [" ++ pretty (ctx, tyT) ++ "])"
                TmProj _ t1 l -> pretty (ctx, t1) ++ "." ++ pretty l
                TmRecord _ fields ->
                        let pf i (li, ti) =
                                (if show li /= show i then pretty li ++ "=" else "") ++ pretty (ctx, ti)
                            pfs i l = case l of
                                [] -> ""
                                [f] -> pf i f
                                f : rest -> pf i f ++ ", " ++ pfs (i + 1) rest
                         in "{" ++ pfs 1 fields ++ "}"
                TmTag _ li ts1 tyT2 ->
                        let prettyArg t =
                                let pptm = pretty (ctx, t)
                                 in if ' ' `elem` pptm then "(" ++ pptm ++ ")" else pptm
                         in pretty li ++ if null ts1 then "" else " " ++ unwords (map prettyArg ts1)
                TmCase _ t1 alts ->
                        let prettyAlt ctx (li, (ki, ti)) =
                                let (xs, ctx') = (`runState` ctx) $
                                        forM [1 .. ki] $ \i -> state $ \ctx -> pickFreshName (str2varName $ show i) ctx
                                 in pretty li ++ " " ++ unwords (map pretty xs) ++ " -> " ++ pretty (ctx', ti)
                         in "(case " ++ pretty (ctx, t1) ++ " of {" ++ intercalate " | " (map (prettyAlt ctx) alts) ++ "})"

prettyAlt :: Context -> (Name, (Int, Term)) -> String
prettyAlt ctx (li, (ki, ti)) =
        let (xs, ctx') = (`runState` ctx) $
                forM [1 .. ki] $ \i -> state $ \ctx -> pickFreshName (str2varName $ show i) ctx
         in pretty li ++ " " ++ unwords (map pretty xs) ++ " -> " ++ pretty (ctx', ti)

instance Pretty (Context, Ty) where
        pretty (ctx, ty) = case ty of
                TyVar _ x n -> if length ctx == n then pretty $ index2name ctx x else "[bad index]"
                TyArr _ tyT1 tyT2 -> "(" ++ pretty (ctx, tyT1) ++ " -> " ++ pretty (ctx, tyT2) ++ ")"
                TyAbs _ tyX knK1 tyT2 ->
                        let (tyX', ctx') = pickFreshName tyX ctx
                         in "(λ" ++ pretty tyX' ++ ": " ++ pretty (ctx, knK1) ++ ". " ++ pretty (ctx', tyT2) ++ ")"
                TyApp _ tyT1 tyT2 -> "(" ++ pretty (ctx, tyT1) ++ " " ++ pretty (ctx, tyT2) ++ ")"
                TyAll _ tyX knK1 tyT2 ->
                        let (tyX', ctx') = pickFreshName tyX ctx
                         in "(∀" ++ pretty tyX' ++ ": " ++ pretty (ctx, knK1) ++ ". " ++ pretty (ctx', tyT2) ++ ")"
                TyRec _ x knK1 tyT2 ->
                        let (x', ctx') = pickFreshName x ctx
                         in "(μ" ++ pretty x' ++ ": " ++ pretty (ctx, knK1) ++ ". " ++ pretty (ctx', tyT2) ++ ")"
                TyId _ b -> pretty b
                TyRecord _ fields ->
                        let pf i (li, tyTi) = (if show li /= show i then pretty li ++ ":" else "") ++ pretty (ctx, tyTi)
                            pfs i l = case l of
                                [] -> ""
                                [f] -> pf i f
                                f : rest -> pf i f ++ ", " ++ pfs (i + 1) rest
                         in "{" ++ pfs 1 fields ++ "}"
                TyVariant _ fields ->
                        let pf (li, tys) = pretty li ++ if null tys then "" else " " ++ unwords (map (\ty -> pretty (ctx, ty)) tys)
                         in intercalate " | " (map pf fields)

instance Pretty (Context, Kind) where
        pretty (ctx, KnStar) = "*"
        pretty (ctx, KnArr knK1 knK2) = "(" ++ pretty (ctx, knK1) ++ " -> " ++ pretty (ctx, knK2) ++ ")"

instance Pretty (Context, Binding) where
        pretty (ctx, NameBind) = "<NameBind>"
        pretty (ctx, VarBind tyT) = pretty (ctx, tyT)
        pretty (ctx, TyVarBind knK) = pretty (ctx, knK)
        pretty (ctx, TmAbbBind t (Just tyT)) = pretty (ctx, t) ++ " : " ++ pretty (ctx, tyT)
        pretty (ctx, TmAbbBind tyT Nothing) = pretty (ctx, tyT)
        pretty (ctx, TyAbbBind tyT (Just knK)) = pretty (ctx, tyT) ++ " : " ++ pretty (ctx, knK)
        pretty (ctx, TyAbbBind tyT Nothing) = pretty (ctx, tyT)

instance Pretty (Context, Commands) where
        pretty (ctx, Commands imps binds body) =
                "import {\n\t"
                        ++ intercalate "\n\t" (map pretty imps)
                        ++ "\n}\nbinds {\n\t"
                        ++ let pb ctx [] = ([], ctx)
                               pb ctx ((_, (x, b)) : bs) =
                                let (x', ctx') = pickFreshName x ctx
                                    (l, ctx'') = pb ctx' bs
                                 in ((pretty x' ++ " = " ++ pretty (ctx, b)) : l, ctx'')
                               (ppbinds, ctx') = pb ctx binds
                            in intercalate "\n\t" ppbinds ++ "\n}\nmain=" ++ pretty (ctx', body)

----------------------------------------------------------------
-- Info
----------------------------------------------------------------
instance GetInfo Term where
        getInfo (TmVar fi _ _) = fi
        getInfo (TmAbs fi _ _ _) = fi
        getInfo (TmApp fi _ _) = fi
        getInfo (TmTAbs fi _ _ _) = fi
        getInfo (TmTApp fi _ _) = fi
        getInfo (TmLet fi _ _ _) = fi
        getInfo (TmFix fi _) = fi
        getInfo (TmFold fi _) = fi
        getInfo (TmUnfold fi _) = fi
        getInfo (TmProj fi _ _) = fi
        getInfo (TmRecord fi _) = fi
        getInfo (TmTag fi _ _ _) = fi
        getInfo (TmCase fi _ _) = fi

instance GetInfo Ty where
        getInfo (TyVar fi _ _) = fi
        getInfo (TyArr fi _ _) = fi
        getInfo (TyAll fi _ _ _) = fi
        getInfo (TyAbs fi _ _ _) = fi
        getInfo (TyApp fi _ _) = fi
        getInfo (TyRec fi _ _ _) = fi
        getInfo (TyId fi _) = fi
        getInfo (TyRecord fi _) = fi
        getInfo (TyVariant fi _) = fi
