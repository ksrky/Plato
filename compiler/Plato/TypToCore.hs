{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.TypToCore (elabExpr, elabDefn, typToCore, typToCoreExpr) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable
import GHC.Stack
import System.Log.Logger

import Data.Graph
import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Driver.Logger
import Plato.Driver.Monad
import Plato.Syntax.Core qualified as C
import Plato.Syntax.Core.Helper
import Plato.Syntax.Typing qualified as T
import Plato.Syntax.Typing.Helper

elabExpr :: (MonadReader e m, HasUniq e, MonadIO m) => T.Expr 'T.Typed -> m C.Term
elabExpr (T.VarE id) = return $ C.Var id
elabExpr (T.AppE' fun arg) = C.App <$> elabExpr fun <*> elabExpr arg
elabExpr (T.AbsE' id ty exp) = C.Lam <$> ((id,) <$> elabType ty) <*> elabExpr exp
elabExpr (T.TAppE fun tyargs) = do
        t <- elabExpr fun
        tys <- mapM elabType tyargs
        return $ foldl C.App t tys
elabExpr (T.TAbsE qnts exp) = do
        t <- elabExpr exp
        foldrM (\(tv, kn) t -> C.Lam <$> ((T.unTyVar tv,) <$> elabKind kn) <*> pure t) t qnts
elabExpr (T.LetE' bnds body) = C.Let <$> elabBinds bnds <*> elabExpr (unLoc body)
elabExpr (T.CaseE' test _ alts) = do
        idX <- freshIdent $ genName "x"
        idY <- freshIdent $ genName "y"
        alts' <- forM alts $ \(pat, exp) -> do
                (con, args) <- elabPat (unLoc pat)
                t <- mkUnfold $ C.Var idY
                (con,) <$> (C.Box <$> (mkSplits t args =<< elabExpr exp))
        C.Split
                <$> elabExpr test
                <*> pure (idX, idY)
                <*> pure (C.Force $ C.Case (C.Var idX) alts')

elabPat ::
        (HasCallStack, MonadReader e m, HasUniq e, MonadIO m) =>
        T.Pat ->
        m (C.Label, [(Ident, C.Type)])
elabPat (T.TagP con args) = do
        args' <- mapM (\(arg, ty) -> (arg,) <$> elabType ty) args
        return (nameIdent con, args')
elabPat _ = unreachable "allowed only tagged constructor pattern"

elabType :: (HasCallStack, MonadReader e m, HasUniq e, MonadIO m) => T.Type -> m C.Type
elabType (T.VarT tv) = return $ C.Var (T.unTyVar tv)
elabType (T.ConT tc) = return $ C.Var tc
elabType (T.ArrT arg res) = join $ mkArr <$> elabType (unLoc arg) <*> elabType (unLoc res)
elabType (T.AllT qnts body) =
        mkPis <$> mapM (\(tv, kn) -> (T.unTyVar tv,) <$> elabKind kn) qnts <*> elabType (unLoc body)
elabType (T.AppT fun arg) = C.App <$> elabType (unLoc fun) <*> elabType (unLoc arg)
elabType ty@T.MetaT{} = do
        liftIO $ emergencyM platoLog $ "elaborate MetaT: " ++ show ty
        unreachable "Plato.TypToCore received MetaT"

elabKind :: (HasCallStack, MonadReader e m, HasUniq e, MonadIO m) => T.Kind -> m C.Type
elabKind T.StarK = return C.Type
elabKind (T.ArrK arg res) = do
        idWC <- freshIdent wcName
        C.Q C.Pi <$> ((idWC,) <$> elabKind arg) <*> elabKind res
elabKind kn@T.MetaK{} = do
        liftIO $ emergencyM platoLog $ "Zonking may " ++ show kn
        unreachable "Plato.TypToCore received MetaK"

elabTypDefn :: forall e m. (MonadReader e m, HasUniq e, MonadIO m) => T.TypDefn 'T.Typed -> m [C.Entry]
elabTypDefn (T.DatDefn' (id, _) params constrs) = do
        def <- C.Defn id <$> dataDefn
        conds <- (++) <$> mapM constrDecl constrs <*> mapM constrDefn constrs
        return $ def : conds
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

elabBinds :: (MonadReader e m, HasUniq e, MonadIO m) => SCC (T.Bind 'T.Typed) -> m [C.Entry]
elabBinds bnds = do
        decs <- forM bnds $ \(T.Bind' (id, ty) _) -> C.Decl id <$> elabType ty
        defs <- forM bnds $ \(T.Bind' (id, _) exp) -> C.Defn id <$> elabExpr exp
        return $ toList decs ++ toList defs

elabDefn :: (MonadReader e m, HasUniq e, MonadIO m) => T.Defn 'T.Typed -> m [C.Entry]
elabDefn (T.TypDefn tdefs) = do
        decs <- forM tdefs $ \(T.DatDefn' (id, kn) _ _) -> C.Decl id <$> elabKind kn
        defs <- concat <$> mapM elabTypDefn tdefs
        return $ toList decs ++ toList defs
elabDefn (T.ValDefn bnds) = elabBinds bnds

typToCore :: PlatoMonad m => T.Prog 'T.Typed -> m [C.Entry]
typToCore decs = do
        uref <- getUniq =<< ask
        runReaderT (concat <$> mapM elabDefn decs) uref

typToCoreExpr :: PlatoMonad m => T.LExpr 'T.Typed -> m C.Term
typToCoreExpr exp = do
        uref <- getUniq =<< ask
        runReaderT (elabExpr $ unLoc exp) uref