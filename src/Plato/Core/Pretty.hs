module Plato.Core.Pretty where

import Plato.Common.Name
import Plato.Core.Syntax

class PrettyCore a where
        pretty :: a -> String

instance PrettyCore Term where
        pretty (TmFloat _ f) = show f
        pretty (TmString _ s) = show s
        pretty (TmTag _ l ts1 _) =
                let prettyArg t =
                        let pptm = pretty t
                         in if ' ' `elem` pptm then "(" ++ pptm ++ ")" else pptm
                 in name2str l ++ if null ts1 then "" else " " ++ unwords (map prettyArg ts1)
        pretty t = error $ "simplification failed.\n" ++ show t