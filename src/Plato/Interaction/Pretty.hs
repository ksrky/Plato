{-# LANGUAGE OverloadedStrings #-}

module Plato.Interaction.Pretty where

import qualified Data.Text as T
import Plato.Common.Error
import Plato.Common.GlbName
import Plato.Common.Name
import Plato.Core.Context
import Plato.Syntax.Core
import Prettyprinter

hsep' :: [Doc ann] -> Doc ann
hsep' docs = if null docs then emptyDoc else emptyDoc <+> hsep docs

ppr :: Context -> Term -> Doc ann
ppr ctx t = pprtm t <> line
    where
        pprtm :: Term -> Doc ann
        pprtm t =
                case t of
                        TmVar x n -> if length ctx == n then pretty $ index2name ctx x else unreachable "Something unexpected occured."
                        TmApp (TmFold _) t2 | isTag t2 -> pprtm t2
                        TmApp{} -> pprapp t
                        TmTag op [t1, t2] _ | isConOp op -> pprtm1 t1 <+> pretty op <+> pprtm1 t2
                        TmTag li ts _ -> hcat [pretty li, hsep' (map pprtm1 ts)]
                        _ -> unreachable "Something unexpected occured."
        isTag :: Term -> Bool
        isTag TmTag{} = True
        isTag _ = False

        isConOp :: GlbName -> Bool
        isConOp = (':' ==) . T.head . nameText . g_name

        pprtm1 :: Term -> Doc ann
        pprtm1 t@TmVar{} = pprtm t
        pprtm1 (TmApp (TmFold _) t2) | isTag t2 = pprtm1 t2
        pprtm1 t@(TmTag _ as _) | null as = pprtm t
        pprtm1 t = parens $ pprtm t

        pprapp :: Term -> Doc ann
        pprapp t = walk t []
            where
                walk :: Term -> [Term] -> Doc ann
                walk (TmApp t1 t2) ts = walk t1 (t2 : ts)
                walk t' ts = pprtm1 t' <+> sep (map pprtm1 ts)

data List a = Nil | a ::: List a

data T = T1 | T2

infixr 5 :::
l :: List T
l = T1 ::: (T2 ::: Nil)