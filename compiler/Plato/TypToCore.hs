{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.TypToCore (typToCore, typToCoreExpr) where

import Control.Monad.Reader
import Data.Foldable
import GHC.Stack
import System.Log.Logger

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Core.Utils
import Plato.Driver.Logger
import Plato.Driver.Monad
import Plato.Syntax.Core qualified as C
import Plato.Syntax.Typing qualified as T
import Plato.Typing.Env

elabExpr :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => T.Expr 'T.Typed -> m C.Term
elabExpr (T.VarE id) = return $ C.Var id
elabExpr (T.AppE fun arg) = C.App <$> elabExpr (unLoc fun) <*> elabExpr (unLoc arg)
elabExpr (T.AbsEok id ty exp) = C.Lam <$> ((id,) <$> elabType ty) <*> elabExpr exp
elabExpr (T.TAppE fun tyargs) = do
        t <- elabExpr fun
        tys <- mapM elabType tyargs
        return $ foldl C.App t tys
elabExpr (T.TAbsE qnts exp) = do
        t <- elabExpr exp
        foldrM (\(tv, kn) t -> C.Lam <$> ((T.unTyVar tv,) <$> elabKind kn) <*> pure t) t qnts
elabExpr (T.LetEok fbnds fspcs body) = C.Let <$> elabFunDecls fbnds fspcs <*> elabExpr (unLoc body)
elabExpr (T.CaseEok match _ alts) = do
        idX <- freshIdent $ genName "x"
        idY <- freshIdent $ genName "y"
        alts <- forM alts $ \(pat, exp) -> do
                (con, vars) <- elabPat (unLoc pat)
                t <- mkUnfold $ C.Var idY
                (con,) <$> (mkSplits t vars =<< elabExpr (unLoc exp))
        C.Split
                <$> elabExpr (unLoc match)
                <*> pure (idX, (idY, C.Case (C.Var idX) alts))

elabPat :: (HasCallStack, MonadIO m) => T.Pat -> m (C.Label, [Ident])
elabPat (T.ConP con pats) = do
        let vars = [id | L _ (T.VarP id) <- pats]
        unless (length pats == length vars) $ do
                liftIO $ errorM platoLog $ show pats
                unreachable "allowed only variable patterns"
        return (nameIdent con, vars)
elabPat _ = unreachable "allowed only constructor pattern"

elabType :: (HasCallStack, MonadReader ctx m, HasUniq ctx, MonadIO m) => T.Type -> m C.Type
elabType (T.VarT tv) = return $ C.Var (T.unTyVar tv)
elabType (T.ConT tc) = return $ C.Var tc
elabType (T.ArrT arg res) = join $ mkArr <$> elabType (unLoc arg) <*> elabType (unLoc res)
elabType (T.AllT qnts body) =
        mkPis <$> mapM (\(tv, kn) -> (T.unTyVar tv,) <$> elabKind kn) qnts <*> elabType (unLoc body)
elabType (T.AppT fun arg) = C.App <$> elabType (unLoc fun) <*> elabType (unLoc arg)
elabType ty@T.MetaT{} = do
        liftIO $ emergencyM platoLog $ "elaborate MetaT: " ++ show ty
        unreachable "Plato.TypToCore received MetaT"

elabKind :: (HasCallStack, MonadReader ctx m, HasUniq ctx, MonadIO m) => T.Kind -> m C.Type
elabKind T.StarK = return C.Type
elabKind (T.ArrK arg res) = do
        idWC <- freshIdent wcName
        C.Q C.Pi <$> ((idWC,) <$> elabKind arg) <*> elabKind res
elabKind kn@T.MetaK{} = do
        liftIO $ emergencyM platoLog $ "Zonking may " ++ show kn
        unreachable "Plato.TypToCore received MetaK"

elabFunDecls :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => [(Ident, T.LExpr 'T.Typed)] -> [(Ident, T.LType)] -> m C.Prog
elabFunDecls fbnds fspcs = do
        fspcs' <- mapM (\(id, ty) -> C.Decl id <$> elabType (unLoc ty)) fspcs
        fbnds' <- mapM (\(id, exp) -> C.Defn id <$> elabExpr (unLoc exp)) fbnds
        return $ fspcs' ++ fbnds'

elabDecl :: forall ctx m. (MonadReader ctx m, HasUniq ctx, MonadIO m) => T.Decl 'T.Typed -> m [C.Entry]
elabDecl (T.SpecDecl (T.TypSpec id kn)) = do
        liftIO $ debugM platoLog $ "elaborate " ++ show id ++ ": " ++ show kn
        kn' <- elabKind kn
        return [C.Decl id kn']
elabDecl (T.SpecDecl (T.ValSpec id ty)) = do
        liftIO $ debugM platoLog $ "elaborate " ++ show id ++ ": " ++ show ty
        ty' <- elabType $ unLoc ty
        return [C.Decl id ty']
elabDecl (T.DefnDecl (T.DatDefnok id _ params constrs)) = do
        liftIO $ debugM platoLog $ "elaborate DatDefnDecl: " ++ show id
        (:) <$> (C.Defn id <$> dataDefn) <*> ((++) <$> mapM constrDecl constrs <*> mapM constrDefn constrs)
    where
        labDefns :: m (Ident, C.Type) = do
                idL <- freshIdent $ genName "l"
                return (idL, C.Enum (map (nameIdent . fst) constrs))
        caseAlts :: m [(Name, C.Type)] = forM constrs $ \(con, ty) -> do
                mapM elabType (fst $ splitConstrTy $ unLoc ty) >>= \case
                        [] -> return (nameIdent con, tUnit)
                        tys -> (nameIdent con,) . C.Rec . C.Box <$> mkTTuple tys
        dataDefn :: m C.Term = do
                idL <- fst <$> labDefns
                C.Q C.Sigma <$> labDefns <*> (C.Case (C.Var idL) <$> caseAlts)
        constrDecl :: (Ident, T.LType) -> m C.Entry
        constrDecl (con, ty) = C.Decl con <$> elabType (unLoc ty)
        constrDefn :: (Ident, T.LType) -> m C.Entry
        constrDefn (con, ty) = do
                let walk :: [Ident] -> T.Type -> m C.Term
                    walk [] (T.AllT qnts body) = do
                        body' <- walk [] $ unLoc body
                        foldrM (\(tv, kn) t -> C.Lam <$> ((T.unTyVar tv,) <$> elabKind kn) <*> pure t) body' qnts
                    walk args (T.ArrT fun arg) = do
                        id <- freshIdent $ str2genName $ show (length args)
                        C.Lam <$> ((id,) <$> elabType (unLoc fun)) <*> walk (id : args) (unLoc arg)
                    walk [] _ = return $ C.Pair (C.Label $ nameIdent con) unit
                    walk args _ = do
                        return $ C.Pair (C.Label (nameIdent con)) (C.Fold $ foldl1 (flip C.Pair) (map C.Var args))
                C.Defn con <$> walk [] (T.AllT params ty)
elabDecl (T.DefnDecl (T.TypDefn id ty)) = do
        liftIO $ debugM platoLog $ "elaborate " ++ show id ++ ": " ++ show ty
        ty' <- elabType $ unLoc ty
        return [C.Defn id ty']
elabDecl (T.DefnDecl (T.FunDefnok id exp)) = do
        liftIO $ debugM platoLog $ "elaborate " ++ show id ++ ": " ++ show exp
        exp' <- elabExpr $ unLoc exp
        return [C.Defn id exp']

typToCore :: PlatoMonad m => T.Program 'T.Typed -> m [C.Entry]
typToCore decs = do
        uref <- getUniq =<< ask
        runReaderT (concat <$> mapM elabDecl decs) uref

typToCoreExpr :: PlatoMonad m => T.LExpr 'T.Typed -> m C.Term
typToCoreExpr exp = do
        uref <- getUniq =<< ask
        runReaderT (elabExpr $ unLoc exp) uref