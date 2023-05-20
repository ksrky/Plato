module Plato.Core.Elab where

import Plato.Common.Name
import Plato.Syntax.Core

fixComb :: Type -> Kind -> Term
fixComb tyT knK =
        TmAbs
                Dummy
                tyT
                ( TmApp
                        ( TmAbs
                                Dummy
                                (TyRec Dummy knK (TyFun (TyVar 0 Dummy) tyT))
                                (TmApp (TmVar 1 Dummy) (TmApp (TmVar 0 Dummy) (TmVar 0 Dummy)))
                        )
                        ( TmAbs
                                Dummy
                                (TyRec Dummy knK (TyFun (TyVar 0 Dummy) tyT))
                                (TmApp (TmVar 1 Dummy) (TmApp (TmVar 0 Dummy) (TmVar 0 Dummy)))
                        )
                )

recursiveBinds :: [(Name, Term)] -> [(Name, Type)] -> [(NameInfo, Term, Type)]
recursiveBinds bnds spcs =
        let t = TmFix (TmAbs Dummy (TyRecord spcs) (TmRecord bnds))
            rbnd = (Dummy, t, TyRecord spcs)
         in foldl
                ( \acc (xi, tyTi) ->
                        let i = length acc - 1
                         in (mkInfoFromName xi, TmProj (TmVar i Dummy) i, tyTi) : acc
                )
                [rbnd]
                spcs

constrsToVariant :: [Name] -> [(Name, Type)] -> Type
constrsToVariant params constrs = undefined
    where
        split :: [Type] -> Type -> [Type] -- tmp
        split acc (TyFun arg res) = split (arg : acc) res
        split acc _ = acc