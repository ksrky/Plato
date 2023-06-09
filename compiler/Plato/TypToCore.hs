{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.TypToCore (elabDecls, typ2core) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import GHC.Stack

import Data.List qualified
import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Driver.Monad
import Plato.Syntax.Core qualified as C
import Plato.Syntax.Typing qualified as T

elabExpr ::
        (HasCallStack, MonadReader ctx m) =>
        T.Expr 'T.TcDone ->
        m C.Term
elabExpr (T.VarE var) = return $ C.Var (ident2text var)
elabExpr (T.AppE fun arg) = C.App <$> elabExpr (unLoc fun) <*> elabExpr (unLoc arg)
elabExpr (T.AbsEok var ty body) = do
        tyT1 <- elabType ty
        t2 <- extendNameWith (nameIdent var) $ elabExpr body
        return $ C.Lam (name2text var, t2)
elabExpr (T.TAppE fun argtys) = do
        t1 <- elabExpr fun
        tys2 <- mapM elabType argtys
        return $ foldl C.App t1 tys2
elabExpr (T.TAbsE qnts body) = do
        qnts' <- forM qnts $ \(tv, kn) -> return (T.unTyVar tv, elabKind kn)
        t1 <- extendNameListWith (map (nameIdent . fst) qnts') $ elabExpr body
        return $ foldr (\(id, _) -> C.Lam (ident2text id)) t1 qnts'
elabExpr (T.LetEok bnds spcs body) = do
        rbnds <- elabFunDecls bnds spcs
        t2 <- extendNameListWith (map (C.actualName . fst) rbnds) $ elabExpr (unLoc body)
        -- return $ foldr (\(xi, ti, _) -> C.TmLet xi ti) t2 rbnds
        return $ foldr (\(xi, (ti, tyTi)) t -> C.TmApp (C.TmAbs xi tyTi t) ti) t2 rbnds
elabExpr (T.CaseEok match ty alts) = do
        t <- elabExpr (unLoc match)
        tyT <- elabType ty
        alts' <- forM alts $ \(pat, exp) -> case unLoc pat of
                T.WildP -> unreachable ""
                T.VarP{} -> unreachable ""
                T.ConP c ps -> do
                        let xs = [nameIdent id | L _ (T.VarP id) <- ps]
                        (nameIdent c,) <$> extendNameListWith xs (elabExpr (unLoc exp))
        return $ C.TmCase (C.TmApp (C.TmUnfold tyT) t) alts'

elabType :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => T.Type -> m C.Type
elabType (T.VarT tv) = do
        i <- getVarIndex (nameIdent $ T.unTyVar tv)
        return $ C.TyVar i (C.mkInfo $ T.unTyVar tv)
elabType (T.ConT tc) = do
        i <- getVarIndex (nameIdent tc)
        return $ C.TyVar i (C.mkInfo tc)
elabType (T.ArrT ty1 ty2) = C.TyFun <$> elabType (unLoc ty1) <*> elabType (unLoc ty2)
elabType (T.AllT qnts ty) = do
        args <- forM qnts $ \(tv, kn) -> return (T.unTyVar tv, elabKind kn)
        tyT2 <- extendNameListWith (map (nameIdent . fst) args) $ elabType (unLoc ty)
        return $ foldr (\(x, knK1) -> C.TyAll (C.mkInfo x) knK1) tyT2 args
elabType (T.AppT ty1 ty2) = C.TyApp <$> elabType (unLoc ty1) <*> elabType (unLoc ty2)
elabType T.MetaT{} = unreachable "Zonking failed"

elabKind :: HasCallStack => T.Kind -> C.Kind
elabKind T.StarK = C.KnStar
elabKind (T.ArrK kn1 kn2) = C.KnFun (elabKind kn1) (elabKind kn2)
elabKind T.MetaK{} = unreachable "elabKind passed T.MetaK"

elabFunDecls ::
        (MonadReader ctx m, HasCoreEnv ctx) =>
        [(Ident, T.LExpr 'T.TcDone)] ->
        [(Ident, T.LType)] ->
        m [(C.NameInfo, (C.Term, C.Type))]
elabFunDecls fbnds fspcs = do
        let proj :: Int -> C.Term
            proj = C.TmProj (C.TmVar 0 C.Dummy)
        fspcs' <- mapM (\(id, ty) -> (id,) <$> elabType (unLoc ty)) fspcs
        fbnds' <- forM fbnds $ \(id, exp) -> case Data.List.findIndex ((== id) . fst) fspcs of
                Just i -> do
                        t <- elabExpr (T.AbsEok id (unLoc $ snd (fspcs !! i)) (unLoc exp))
                        return (nameIdent id, C.TmApp t (proj i))
                Nothing -> unreachable "function not defined"
        return $ recursiveBinds fbnds' fspcs'

elabDecls :: forall ctx m. (MonadReader ctx m, HasCoreEnv ctx) => [T.Decl 'T.TcDone] -> m [C.Command]
elabDecls decs = do
        let fbnds = [(id, exp) | T.BindDecl (T.FunBindok id exp) <- decs]
            domains = map fst fbnds
        (fspcs, rest) <- execWriterT $ forM decs $ \case
                T.SpecDecl (T.ValSpec id ty) | id `elem` domains -> tell ([(id, ty)], [])
                dec -> tell ([], [dec])
        let elabDecls' :: [T.Decl 'T.TcDone] -> m [C.Command]
            elabDecls' (T.SpecDecl T.TypSpec{} : rest) = elabDecls' rest
            elabDecls' (T.BindDecl (T.DatBindok id kn params constrs) : rest) = do
                let knK = elabKind kn
                extendNameWith (nameIdent id) $ do
                        let elabConstrs :: [(Ident, T.LType)] -> m [(Ident, C.Type)]
                            elabConstrs [] = return []
                            elabConstrs ((con, ty) : cs) = do
                                c <- (con,) <$> elabType (T.AllT params ty)
                                cs' <- extendNameWith (nameIdent con) $ elabConstrs cs
                                return $ c : cs'
                        fun_constrs <- elabConstrs constrs
                        var_constrs <- mapM (\(con, ty) -> (con,) <$> elabType (unLoc ty)) constrs
                        let params' = map (\(tv, kn) -> (C.mkInfo $ T.unTyVar tv, elabKind kn)) params
                            tyT_nonrec = foldr (uncurry C.TyAbs) (constrsToVariant var_constrs) params'
                            tyT_rec = C.TyRec (C.mkInfo id) knK tyT_nonrec
                        rest' <- extendNameListWith (map (nameIdent . fst) constrs) $ elabDecls' rest
                        let constrBinds :: Int -> [(Ident, C.Type)] -> [C.Command]
                            constrBinds _ [] = []
                            constrBinds n_con ((con, tyT) : rest) =
                                let walk :: Int -> C.Type -> C.Term
                                    walk (-1) (C.TyAll x knK1 tyT2) = C.TmTAbs x knK1 (walk (-1) tyT2)
                                    walk n_arg (C.TyFun tyT1 tyT2) = C.TmAbs C.Dummy tyT1 (walk (n_arg + 1) tyT2)
                                    walk n_arg _ =
                                        C.TmApp (C.TmFold (C.TyVar (n_con + n_arg + 1) (C.mkInfo id))) $
                                                C.TmInj
                                                        n_con
                                                        (shift (n_con + n_arg + 1) tyT_nonrec)
                                                        (map (\i -> C.TmVar (n_arg - i) C.Dummy) [0 .. n_arg])
                                 in C.Bind (C.mkInfo con) (C.TmAbbBind (walk (-1) tyT) tyT) : constrBinds (n_con + 1) rest
                        return $ C.Bind (C.mkInfo id) (C.TyAbbBind tyT_rec knK) : constrBinds 0 fun_constrs ++ rest'
            elabDecls' (T.BindDecl (T.TypBind id ty) : rest) = do
                -- TODO: remove
                tyT <- elabType (unLoc ty)
                knK <- getKind =<< getVarIndex (nameIdent id)
                rest' <- elabDecls' rest
                return $ C.Bind (C.mkInfo id) (C.TyAbbBind tyT knK) : rest'
            elabDecls' (T.SpecDecl (T.ValSpec id ty) : rest) = do
                tyT <- elabType (unLoc ty)
                rest' <- extendWith (nameIdent id) (C.TmVarBind tyT) $ elabDecls' rest
                return $ C.Bind (C.mkInfo id) (C.TmVarBind tyT) : rest'
            elabDecls' _ = do
                rbnds <- elabFunDecls fbnds fspcs
                return $ map (\(id, (t, tyT)) -> C.Bind id (C.TmAbbBind t tyT)) rbnds
        elabDecls' rest

typ2core :: PlatoMonad m => T.Program 'T.TcDone -> m [C.Command]
typ2core decs = do
        let cmds = runReader (elabDecls decs) initCoreEnv
        return cmds