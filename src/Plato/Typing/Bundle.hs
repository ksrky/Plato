{-# LANGUAGE TupleSections #-}

module Plato.Typing.Bundle where

import Plato.Syntax.Typing
import Plato.Types.Location
import Plato.Types.Name
import Plato.Types.Name.Global

import Control.Exception.Safe
import Control.Monad.Reader
import Plato.Types.Error (unreachable)

type Level = Int

data Env = Env {env_modn :: ModuleName, env_level :: Level, env_names :: [Name]}

getWrapperName :: Monad m => ReaderT Env m Name
getWrapperName = do
        lev <- asks env_level
        return $ str2conName (":l" ++ show lev)

mkValue :: MonadThrow m => GlbName -> ReaderT Env m Expr
mkValue glbn = case g_sort glbn of
        External modn -> return $ ProjE (VarE $ newGlbName Local $ modn2name modn) glbn
        Internal -> do
                modn <- asks env_modn
                return $ ProjE (VarE $ newGlbName Local $ modn2name modn) glbn
        Local -> do
                names <- asks env_names
                n <- getWrapperName
                if g_name glbn `elem` names
                        then return $ ProjE (VarE $ newGlbName Local n) glbn
                        else return $ VarE glbn

-- | Renaming
bundle :: MonadThrow m => Expr -> ReaderT Env m Expr
bundle (VarE x) = case nameSpace (g_name x) of
        VarName -> mkValue x
        ConName -> return $ VarE x
        _ -> unreachable ""
bundle (AbsE x mty e) = AbsE x mty <$> bundle e
bundle (AppE e1 e2) = AppE <$> bundle e1 <*> bundle e2
bundle (TAbsE xs e) = TAbsE xs <$> bundle e
bundle (TAppE e tys) = TAppE <$> bundle e <*> pure tys
bundle (LetE decs body) = do
        lev <- asks env_level
        dec <- local (\s -> s{env_level = lev + 1}) $ bundleFuncDs decs
        names <- forM decs $ \(FuncD var _ _) -> return $ unLoc var
        body' <- local (\s -> s{env_level = lev + 1, env_names = names}) $ bundle body
        return $ LetE [dec] body'
bundle (CaseE e mty alts) = do
        e' <- bundle e
        alts' <- forM alts $ \(pat, body) -> (pat,) <$> bundle body
        return $ CaseE e' mty alts'
bundle e = return e

bundleFuncDs :: MonadThrow m => [FuncD] -> ReaderT Env m FuncD
bundleFuncDs decs = do
        lev <- asks env_level
        let r = str2conName (":l" ++ show lev)
        names <- forM decs $ \(FuncD var _ _) -> return $ unLoc var
        fields <- forM decs $ \(FuncD var body _) -> do
                body' <- local (\s -> s{env_names = names}) $ bundle body
                return (internalName var, body')
        let fieldtys = [(internalName var, ty) | FuncD var _ ty <- decs]
        return $ FuncD (noLoc r) (RecordE fields) (RecordT fieldtys)

bundleEval :: MonadThrow m => ModuleName -> Expr -> m Expr
bundleEval modn = (`runReaderT` Env modn 0 []) . bundle

bundleTopFuncDs :: MonadThrow m => ModuleName -> [FuncD] -> m FuncD
bundleTopFuncDs modn decs = do
        let r = noLoc $ modn2name modn
        fields <- forM decs $ \(FuncD var body _) -> do
                body' <- runReaderT (bundle body) (Env modn 0 [])
                return (internalName var, body')
        let fieldtys = [(internalName var, ty) | FuncD var _ ty <- decs]
        return $ FuncD r (RecordE fields) (RecordT fieldtys)