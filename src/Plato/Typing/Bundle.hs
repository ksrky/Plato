{-# LANGUAGE TupleSections #-}

module Plato.Typing.Bundle where

import Plato.Syntax.Typing
import Plato.Types.Error
import Plato.Types.Location
import Plato.Types.Name
import Plato.Types.Name.Global

import Control.Exception.Safe
import Control.Monad.Reader

mkValue :: MonadThrow m => GlbName -> ReaderT Level m Expr
mkValue glbn = case g_sort glbn of
        External modn -> return $ ProjE (VarE $ newGlbName Local $ modn2name modn) glbn --tmp: Local
        Internal lev -> return $ ProjE (VarE $ newGlbName Local $ str2conName (":l" ++ show lev)) glbn --tmp: Local
        Local -> return $ VarE glbn

-- | Renaming
class Bundle a where
        bundle :: MonadThrow m => a -> ReaderT Level m a

instance Bundle Expr where
        bundle (VarE x) = case nameSpace (g_name x) of
                VarName -> mkValue x
                ConName -> return $ VarE x
                _ -> unreachable ""
        bundle (AbsE x mty e) = AbsE x mty <$> bundle e
        bundle (AppE e1 e2) = AppE <$> bundle e1 <*> bundle e2
        bundle (TAbsE xs e) = TAbsE xs <$> bundle e
        bundle (TAppE e tys) = TAppE <$> bundle e <*> pure tys
        bundle (LetE decs body) = do
                dec <- local (+ 1) $ bundleFuncDs decs
                body' <- bundle body
                return $ LetE [dec] body'
        bundle (CaseE e mty alts) = do
                e' <- bundle e
                alts' <- forM alts $ \(pat, body) -> (pat,) <$> bundle body
                return $ CaseE e' mty alts'
        bundle e = return e

instance Bundle Type where
        bundle (RecT x kn ty) = return $ RecT x kn (bundle' ty)
            where
                bundle' :: Type -> Type
                bundle' (ConT con)
                        | unLoc x == g_name con = ConT con{g_sort = Local}
                        | otherwise = ConT con
                bundle' (ArrT ty1 ty2) = ArrT (bundle' ty1) (bundle' ty2)
                bundle' (AllT xs ty) = AllT xs (bundle' ty)
                bundle' (AbsT x mkn ty) = AbsT x mkn (bundle' ty)
                bundle' (AppT ty1 ty2) = AppT (bundle' ty1) (bundle' ty2)
                bundle' (SumT fieldtys) = SumT $ map (\(l, tys) -> (l, map bundle' tys)) fieldtys
                bundle' ty = ty
        bundle ty = return ty

renameRecT :: MonadThrow m => Type -> m Type
renameRecT = (`runReaderT` 0) . bundle

bundleFuncDs :: MonadThrow m => [FuncD] -> ReaderT Level m FuncD
bundleFuncDs decs = do
        lev <- ask
        let r = str2conName (":l" ++ show lev)
        fields <- forM decs $ \(FuncD var body _) -> do
                body' <- bundle body
                return (internalName lev var, body')
        let fieldtys = [(internalName lev var, ty) | FuncD var _ ty <- decs]
        return $ FuncD (noLoc r) (RecordE fields) (RecordT fieldtys)

bundleEval :: MonadThrow m => Expr -> m Expr
bundleEval = (`runReaderT` 0) . bundle

bundleTopFuncDs :: MonadThrow m => ModuleName -> [FuncD] -> m FuncD
bundleTopFuncDs modn decs = do
        let r = noLoc $ modn2name modn
        fields <- forM decs $ \(FuncD var body _) -> do
                body' <- runReaderT (bundle body) 0
                return (externalName modn var, body')
        let fieldtys = [(externalName modn var, ty) | FuncD var _ ty <- decs]
        return $ FuncD r (RecordE fields) (RecordT fieldtys)