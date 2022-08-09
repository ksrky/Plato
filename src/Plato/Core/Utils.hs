module Plato.Core.Utils where

import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Syntax

----------------------------------------------------------------
-- Pretty
----------------------------------------------------------------
class PrettyCore a where
        pretty :: Context -> a -> String

instance PrettyCore Term where
        pretty ctx t = case t of
                TmVar _ x n -> if length ctx == n then show $ index2name ctx x else "[bad index]"
                TmAbs _ x tyT1 t2 ->
                        let (x', ctx') = pickfreshname x ctx
                         in "(\\" ++ show x' ++ ": " ++ pretty ctx tyT1 ++ ". " ++ pretty ctx' t2 ++ ")"
                TmApp _ t1 t2 -> "(" ++ pretty ctx t1 ++ " " ++ pretty ctx t2 ++ ")"
                TmTAbs _ tyX knK1 t2 ->
                        let (tyX', ctx') = pickfreshname tyX ctx
                         in "(\\" ++ show tyX' ++ ": " ++ pretty ctx knK1 ++ ". " ++ pretty ctx' t2 ++ ")"
                TmTApp _ t1 tyT2 -> "(" ++ pretty ctx t1 ++ " [" ++ pretty ctx tyT2 ++ "]" ++ ")"
                TmFloat _ f -> show f
                TmString _ s -> show s
                TmTag _ li ts1 tyT2 ->
                        let prettyArg t =
                                let pptm = pretty ctx t
                                 in if ' ' `elem` pptm then "(" ++ pptm ++ ")" else pptm
                         in name2str li ++ if null ts1 then "" else " " ++ unwords (map prettyArg ts1)
                _ -> error $ "simplification failed.\n" ++ show t

instance PrettyCore Ty where
        pretty ctx ty = case ty of
                TyVar _ x n -> if length ctx == n then show $ index2name ctx x else "[bad index]"
                TyArr _ tyT1 tyT2 -> "(" ++ pretty ctx tyT1 ++ " -> " ++ pretty ctx tyT2 ++ ")"
                TyAbs _ tyX knK1 tyT2 ->
                        let (tyX', ctx') = pickfreshname tyX ctx
                         in "(λ" ++ show tyX' ++ ": " ++ pretty ctx knK1 ++ ". " ++ pretty ctx' tyT2 ++ ")"
                TyApp _ tyT1 tyT2 -> "(" ++ pretty ctx tyT1 ++ " " ++ pretty ctx tyT2 ++ ")"
                TyAll _ tyX knK1 tyT2 ->
                        let (tyX', ctx') = pickfreshname tyX ctx
                         in "(∀" ++ show tyX' ++ ": " ++ pretty ctx knK1 ++ ". " ++ pretty ctx' tyT2 ++ ")"
                TyVariant fields -> undefined

instance PrettyCore Kind where
        pretty ctx KnStar = "*"
        pretty ctx (KnArr knK1 knK2) = "(" ++ pretty ctx knK1 ++ " -> " ++ pretty ctx knK2 ++ ")"

----------------------------------------------------------------
-- Info
----------------------------------------------------------------
class GetInfo a where
        getInfo :: a -> Info

instance GetInfo Term where
        getInfo (TmVar fi _ _) = fi
        getInfo (TmAbs fi _ _ _) = fi
        getInfo (TmApp fi _ _) = fi
        getInfo (TmTAbs fi _ _ _) = fi
        getInfo (TmTApp fi _ _) = fi
        getInfo (TmFloat fi _) = fi
        getInfo (TmString fi _) = fi
        getInfo (TmLet fi _ _ _) = fi
        getInfo (TmTag fi _ _ _) = fi
        getInfo (TmCase fi _ _) = fi

instance GetInfo Ty where
        getInfo (TyVar fi _ _) = fi
        getInfo (TyArr fi _ _) = fi
        getInfo (TyAll fi _ _ _) = fi
        getInfo (TyAbs fi _ _ _) = fi
        getInfo (TyApp fi _ _) = fi
        getInfo (TyVariant _) = unreachable "TyVariant does not have Info"
