{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.TypToCore where

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Core.Utils
import Plato.Driver.Monad
import Plato.Syntax.Core qualified as C
import Plato.Syntax.Typing qualified as T
import Plato.Typing.Utils

elabExpr :: T.Expr 'T.TcDone -> C.Term
elabExpr (T.VarE id) = C.Var (nameIdent id)
elabExpr (T.AppE fun arg) = C.App (elabExpr $ unLoc fun) (elabExpr $ unLoc arg)
elabExpr (T.AbsEok id ty exp) = C.Lam (nameIdent id, elabType ty) (elabExpr exp)
elabExpr (T.TAppE fun tyargs) = foldl C.App (elabExpr fun) (map elabType tyargs)
elabExpr (T.TAbsE qnts exp) =
        foldr (\(tv, kn) t -> C.Lam (nameIdent $ T.unTyVar tv, elabKind kn) t) (elabExpr exp) qnts
elabExpr (T.LetEok fbnds fspcs body) = C.Let (elabFunDecls fbnds fspcs) (elabExpr $ unLoc body)
elabExpr (T.CaseEok match _ alts) =
        let x = genName "x"
            y = genName "y"
         in C.Split
                (elabExpr $ unLoc match)
                ( x
                ,
                        ( y
                        , C.Case
                                (C.Var x)
                                ( (`map` alts) $ \(pat, exp) ->
                                        let (con, vars) = elabPat (unLoc pat)
                                         in (con, mkSplits (mkUnfold $ C.Var y) vars (elabExpr (unLoc exp)))
                                )
                        )
                )

elabPat :: T.Pat -> (C.Label, [Name])
elabPat (T.ConP con pats) =
        let vars = (`map` pats) $ \case
                L _ (T.VarP id) -> nameIdent id
                _ -> unreachable "allowed only variable patterns"
         in (nameIdent con, vars)
elabPat _ = unreachable "allowed only constructor pattern"

elabType :: T.Type -> C.Type
elabType (T.VarT tv) = C.Var (nameIdent $ T.unTyVar tv)
elabType (T.ConT tc) = C.Var (nameIdent tc)
elabType (T.ArrT arg res) = mkArr (elabType $ unLoc arg) (elabType $ unLoc res)
elabType (T.AllT qnts body) =
        mkPis (map (\(tv, kn) -> (nameIdent $ T.unTyVar tv, elabKind kn)) qnts) (elabType $ unLoc body)
elabType (T.AppT fun arg) = C.App (elabType $ unLoc fun) (elabType $ unLoc arg)
elabType T.MetaT{} = unreachable ""

elabKind :: T.Kind -> C.Type
elabKind T.StarK = C.Type
elabKind (T.ArrK arg res) = C.Q C.Pi (wcName, elabKind arg) (elabKind res)
elabKind T.MetaK{} = unreachable ""

elabFunDecls :: [(Ident, T.LExpr 'T.TcDone)] -> [(Ident, T.LType)] -> C.Prog
elabFunDecls fbnds fspcs =
        map (\(id, ty) -> C.Decl (nameIdent id) (elabType $ unLoc ty)) fspcs
                ++ map (\(id, exp) -> C.Defn (nameIdent id) (elabExpr $ unLoc exp)) fbnds

elabDecl :: T.Decl 'T.TcDone -> [C.Entry]
elabDecl (T.SpecDecl (T.TypSpec id kn)) = [C.Decl (nameIdent id) (elabKind kn)]
elabDecl (T.SpecDecl (T.ValSpec id ty)) = [C.Decl (nameIdent id) (elabType $ unLoc ty)]
elabDecl (T.DefnDecl (T.DatDefnok id _ params constrs)) =
        let labDefns :: (Name, C.Term) = (genName "l", C.Enum (map (nameIdent . fst) constrs))
            caseAlts :: [(Name, C.Type)] = (`map` constrs) $ \(con, ty) -> case map elabType (fst $ splitConstrTy $ unLoc ty) of
                [] -> (nameIdent con, tUnit)
                tys -> (nameIdent con, C.Rec $ C.Box $ mkTTuple tys)
            dataDefn :: C.Term = C.Q C.Sigma labDefns (C.Case (C.Var $ genName "l") caseAlts)
            constrDefn :: (Ident, T.LType) -> C.Entry
            constrDefn (con, ty) =
                let walk :: Int -> T.Type -> C.Term
                    walk 0 (T.AllT qnts body) = foldr (\(tv, kn) -> C.Lam (nameIdent (T.unTyVar tv), elabKind kn)) (walk 0 $ unLoc body) qnts
                    walk n_arg (T.ArrT fun arg) = C.Lam (str2genName $ show n_arg, elabType $ unLoc fun) (walk (n_arg + 1) $ unLoc arg)
                    walk 0 _ = C.Pair (C.Label (nameIdent con)) unit
                    walk n_arg _ = C.Pair (C.Label (nameIdent con)) (C.Fold $ foldr1 C.Pair (map (C.Var . str2genName . show) [0 .. n_arg - 1]))
                 in C.Defn (nameIdent con) (walk 0 (T.AllT params ty))
         in C.Defn (nameIdent id) dataDefn : map constrDefn constrs
elabDecl (T.DefnDecl (T.TypDefn id ty)) = [C.Defn (nameIdent id) (elabType $ unLoc ty)]
elabDecl (T.DefnDecl (T.FunDefnok id exp)) = [C.Defn (nameIdent id) (elabExpr $ unLoc exp)]

typ2core :: PlatoMonad m => T.Program 'T.TcDone -> m [C.Entry]
typ2core = return . concatMap elabDecl