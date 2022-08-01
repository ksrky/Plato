module Plato.Debug.PrettyCore where

import Data.List
import Plato.Common.Name
import Plato.Common.Vect
import Plato.Core.Context
import Plato.Core.Syntax

class PrettyCore a where
        prcore :: Context -> a -> String

instance PrettyCore Term where
        prcore ctx t = case t of
                TmVar _ i n ->
                        if length ctx == n
                                then if i < 0 then show i ++ show (vmap fst ctx) else name2str $ fst (ctx ! i)
                                else "<bad index: " ++ show i ++ "/" ++ show (length ctx) ++ " => " ++ show i ++ "/" ++ show n ++ " in " ++ show (vmap fst ctx) ++ ">"
                TmAbs _ (x, tyT1) t2 ->
                        let ctx' = cons (x, NameBind) ctx
                         in "\\(" ++ name2str x ++ ":" ++ prcore ctx' tyT1 ++ "). " ++ prcore ctx' t2
                TmApp _ t1 t2 -> paren (prcore ctx t1) ++ " " ++ paren (prcore ctx t2)
                TmTAbs _ (x, knK1) tyT2 ->
                        let ctx' = cons (x, NameBind) ctx
                         in "/\\(" ++ name2str x ++ ":" ++ prcore ctx' knK1 ++ "). " ++ prcore ctx' tyT2
                TmTApp _ t1 tyT2 -> paren (prcore ctx t1) ++ " [" ++ prcore ctx tyT2 ++ "]"
                TmFloat _ f -> show f
                TmString _ s -> show s
                TmLet _ (x, t1) t2 -> "let (" ++ name2str x ++ " = " ++ prcore ctx t1 ++ ") in " ++ prcore ctx t2
                TmCase _ t alts ->
                        "case " ++ prcore ctx t ++ " of { "
                                ++ intercalate
                                        " | "
                                        ( map
                                                ( \(li, (ki, ti)) ->
                                                        let binds = replicate ki (dummyName, NameBind)
                                                            ctx' = foldr cons ctx binds
                                                         in name2str li ++ " -> " ++ prcore ctx' ti
                                                )
                                                alts
                                        )
                                ++ " }"
                TmTag _ l ts1 _ ->
                        let prettyArg t = paren $ prcore ctx t
                         in name2str l ++ if null ts1 then "[]" else "[" ++ unwords (map prettyArg ts1) ++ "]"

instance PrettyCore Ty where
        prcore ctx ty = case ty of
                TyVar i n ->
                        if length ctx == n
                                then if i < 0 then show i ++ show (vmap fst ctx) else name2str $ fst (ctx ! i)
                                else "<bad index: " ++ show i ++ "/" ++ show (length ctx) ++ " => " ++ show i ++ "/" ++ show n ++ " in " ++ show (vmap fst ctx) ++ ">"
                TyString -> "String"
                TyFloat -> "Float"
                TyVariant fields -> undefined
                TyAbs (x, knK1) tyT2 ->
                        let ctx' = cons (x, NameBind) ctx
                         in "/\\(" ++ name2str x ++ ":" ++ prcore ctx' knK1 ++ "). " ++ prcore ctx' tyT2
                TyArr tyT1 tyT2 -> prcore ctx tyT1 ++ " -> " ++ paren (prcore ctx tyT2)
                TyApp tyT1 tyT2 -> paren (prcore ctx tyT1) ++ " " ++ paren (prcore ctx tyT2)
                TyAll (x, knK1) tyT2 ->
                        let ctx' = cons (x, NameBind) ctx
                         in "forall " ++ name2str x ++ ":" ++ prcore ctx' knK1 ++ ". " ++ prcore ctx' tyT2

instance PrettyCore Kind where
        prcore ctx KnStar = "*"
        prcore ctx (KnArr knK1 knK2) = prcore ctx knK1 ++ paren (prcore ctx knK2)

paren :: String -> String
paren s = if ' ' `elem` s then "(" ++ s ++ ")" else s
