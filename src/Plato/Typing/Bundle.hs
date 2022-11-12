{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plato.Typing.Bundle where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.Name.Global
import Plato.Common.SrcLoc
import Plato.Syntax.Typing

import Control.Exception.Safe
import Control.Monad.Reader

type Level = Int

mkValue :: MonadThrow m => GlbName -> m Expr
mkValue glbn = case g_sort glbn of
        ExportedDef modn -> return $ ProjE (VarE $ systemName $ modn2name modn) glbn
        LocalDef -> return $ VarE glbn
        SystemDef -> throwUnexpectedErr "Invalid namesort: SystemDef"

-- | Renaming
bundle :: MonadThrow m => Expr -> ReaderT Level m Expr
bundle (VarE x) = mkValue x
bundle (AbsE x ty e) = AbsE x ty <$> bundle e
bundle (AppE e1 e2) = AppE <$> bundle e1 <*> bundle e2
bundle (TAbsE xs e) = TAbsE xs <$> bundle e
bundle (TAppE e tys) = TAppE <$> bundle e <*> pure tys
bundle (LetE decs body) = do
        dec <- local (+ 1) $ bundleFuncDs decs
        body' <- bundle body
        return $ LetE [dec] body'
bundle (CaseE e ty alts) = do
        e' <- bundle e
        alts' <- forM alts $ \(pat, body) -> (pat,) <$> bundle body
        return $ CaseE e' ty alts'
bundle e = return e

bundleFuncDs :: MonadThrow m => [FuncD] -> ReaderT Level m FuncD
bundleFuncDs decs = do
        lev <- ask
        let r = noLoc $ str2conName (":l" ++ show lev)
        fields <- forM decs $ \(FuncD var body _) -> do
                body' <- bundle body
                return (var, body')
        let fieldtys = [(var, ty) | FuncD var _ ty <- decs]
        return $ FuncD r (RecordE fields) (RecordT fieldtys)

bundleEval :: MonadThrow m => Expr -> m Expr
bundleEval = (`runReaderT` 1) . bundle

bundleTopFuncDs :: MonadThrow m => ModuleName -> [FuncD] -> m FuncD
bundleTopFuncDs modn decs = do
        let r = noLoc $ modn2name modn
        fields <- forM decs $ \(FuncD var body _) -> do
                body' <- runReaderT (bundle body) 1
                return (var, body')
        let fieldtys = [(var, ty) | FuncD var _ ty <- decs]
        return $ FuncD r (RecordE fields) (RecordT fieldtys)
