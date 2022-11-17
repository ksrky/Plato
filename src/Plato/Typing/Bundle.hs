{-# LANGUAGE TupleSections #-}

module Plato.Typing.Bundle where

import Plato.Syntax.Typing
import Plato.Types.Location
import Plato.Types.Name
import Plato.Types.Name.Global

import Control.Exception.Safe
import Control.Monad.Reader

type Level = Int

mkValue :: MonadThrow m => GlbName -> ReaderT (Level, ModuleName) m Expr
mkValue glbn = case g_sort glbn of
        External modn -> return $ ProjE (VarE $ newGlbName (External modn) $ modn2name modn) glbn
        Internal -> do
                modn <- asks snd
                return $ ProjE (VarE $ newGlbName Internal $ modn2name modn) glbn
        Local -> return $ VarE glbn

-- | Renaming
bundle :: MonadThrow m => Expr -> ReaderT (Level, ModuleName) m Expr
bundle (VarE x) = mkValue x
bundle (AbsE x mty e) = AbsE x mty <$> bundle e
bundle (AppE e1 e2) = AppE <$> bundle e1 <*> bundle e2
bundle (TAbsE xs e) = TAbsE xs <$> bundle e
bundle (TAppE e tys) = TAppE <$> bundle e <*> pure tys
bundle (LetE decs body) = do
        dec <- local (\(l, m) -> (l + 1, m)) $ bundleFuncDs decs
        body' <- bundle body
        return $ LetE [dec] body'
bundle (CaseE e mty alts) = do
        e' <- bundle e
        alts' <- forM alts $ \(pat, body) -> (pat,) <$> bundle body
        return $ CaseE e' mty alts'
bundle e = return e

bundleFuncDs :: MonadThrow m => [FuncD] -> ReaderT (Level, ModuleName) m FuncD
bundleFuncDs decs = do
        lev <- asks fst
        let r = noLoc $ str2conName (":l" ++ show lev)
        fields <- forM decs $ \(FuncD var body _) -> do
                body' <- bundle body
                return (internalName var, body')
        let fieldtys = [(internalName var, ty) | FuncD var _ ty <- decs]
        return $ FuncD r (RecordE fields) (RecordT fieldtys)

bundleEval :: MonadThrow m => ModuleName -> Expr -> m Expr
bundleEval modn = (`runReaderT` (1, modn)) . bundle

bundleTopFuncDs :: MonadThrow m => ModuleName -> [FuncD] -> m FuncD
bundleTopFuncDs modn decs = do
        let r = noLoc $ modn2name modn
        fields <- forM decs $ \(FuncD var body _) -> do
                body' <- runReaderT (bundle body) (1, modn)
                return (internalName var, body')
        let fieldtys = [(internalName var, ty) | FuncD var _ ty <- decs]
        return $ FuncD r (RecordE fields) (RecordT fieldtys)