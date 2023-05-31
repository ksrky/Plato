module Plato.Core.Elab where

import Plato.Common.Ident
import Plato.Common.Name
import Plato.Core.Calc
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

product :: [Term] -> Term
product ts = TmRecord (map (dummyVN,) ts)

productTy :: [Type] -> Type
productTy tys = TyRecord (map (dummyVN,) tys)

recursiveBinds :: [(Name, Term)] -> [(Ident, Type)] -> [(NameInfo, (Term, Type))]
recursiveBinds [] [] = []
recursiveBinds bnds spcs =
        let spcs' = map (\(id, tyT) -> (nameIdent id, tyT)) spcs
            t = TmFix (TmAbs Dummy (TyRecord spcs') (TmRecord bnds))
            rbnd = (Dummy, (t, TyRecord spcs'))
         in reverse $
                foldr
                        ( \(idi, tyTi) acc ->
                                let i = length acc - 1
                                 in (mkInfo idi, (TmProj (TmVar i Dummy) i, shift i tyTi)) : acc
                        )
                        [rbnd]
                        spcs

constrsToVariant :: [(Ident, Type)] -> Type
constrsToVariant constrs = TySum (map ((productTy . split []) . snd) constrs)
    where
        split :: [Type] -> Type -> [Type] -- tmp
        split acc (TyFun arg res) = split (arg : acc) res
        split acc _ = acc

constrBinds :: Ident -> [(Ident, Type)] -> [Command]
constrBinds tyname = loop 0
    where
        loop :: Int -> [(Ident, Type)] -> [Command]
        loop _ [] = []
        loop n_con ((con, tyT) : rest) =
                let t = TmApp (TmFold (TyVar n_con (mkInfo tyname))) (mkConstr n_con tyT)
                 in Bind (mkInfo con) (TmAbbBind t tyT) : loop (n_con + 1) rest

mkConstr :: Int -> Type -> Term
mkConstr n_con = walk (-1)
    where
        walk :: Int -> Type -> Term
        walk (-1) (TyAll x knK1 tyT2) = TmTAbs x knK1 (walk (-1) tyT2)
        walk n_arg (TyFun tyT1 tyT2) = TmAbs Dummy tyT1 (walk (n_arg + 1) tyT2)
        walk n_arg tyT = TmInj n_con tyT (map (\i -> TmVar (n_arg - i) Dummy) [0 .. n_arg])