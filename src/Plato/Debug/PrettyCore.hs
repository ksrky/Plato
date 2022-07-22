module Plato.Debug.PrettyCore where

import Plato.Common.Name
import Plato.Core.Syntax

prettyTerm :: Term -> String
prettyTerm (TmFloat f) = show f
prettyTerm (TmString s) = show s
prettyTerm (TmTag l ts1 _) =
    let prettyArg t =
            let pptm = prettyTerm t in if ' ' `elem` pptm then "(" ++ pptm ++ ")" else pptm
     in name2str l ++ if null ts1 then "" else " " ++ unwords (map prettyArg ts1)
prettyTerm t = error $ "simplification failed.\n" ++ show t
