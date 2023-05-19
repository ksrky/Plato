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

recursiveBinds :: [(Name, Term)] -> [(Name, Type)] -> Term
recursiveBinds bnds spcs =
        let tyT = TyRecord spcs
            t = TmRecord bnds
         in TmApp (fixComb tyT KnStar) t
