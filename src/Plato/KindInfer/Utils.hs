{-# LANGUAGE TupleSections #-}

module Plato.KindInfer.Utils where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S

import Plato.Common.Error
import Plato.KindInfer.Monad
import Plato.Syntax.Typing

zonkKind :: MonadIO m => Kind -> Ki m Kind
zonkKind StarK = return StarK
zonkKind (ArrK kn1 kn2) = do
        kn1' <- zonkKind kn1
        kn2' <- zonkKind kn2
        return (ArrK kn1' kn2')
zonkKind (MetaK kv) = do
        mb_kn <- readMetaKv kv
        case mb_kn of
                Nothing -> return (MetaK kv)
                Just kn -> do
                        kn' <- zonkKind kn
                        writeMetaKv kv kn'
                        return kn'

zonkType :: MonadIO m => Type -> Ki m Type
zonkType (VarT tv) = return $ VarT tv
zonkType (ConT x) = return $ ConT x
zonkType (ArrT arg res) = ArrT <$> zonkType `traverse` arg <*> zonkType `traverse` res
zonkType (AllT tvs ty) = do
        tvs' <- forM tvs $ \(tv, mkn) -> (tv,) <$> zonkKind `traverse` mkn
        AllT tvs' <$> zonkType `traverse` ty
zonkType (AbsT x mkn ty) = AbsT x <$> zonkKind `traverse` mkn <*> zonkType `traverse` ty
zonkType (AppT fun arg) = AppT <$> zonkType `traverse` fun <*> zonkType `traverse` arg
zonkType (RecT x kn ty) = RecT x kn <$> zonkType `traverse` ty
zonkType (RecordT fields) = do
        fields' <- forM fields $ \(x, ty) -> (x,) <$> zonkType `traverse` ty
        return $ RecordT fields'
zonkType (SumT fields) = do
        fields' <- forM fields $ \(x, tys) -> (x,) <$> mapM (zonkType `traverse`) tys
        return $ SumT fields'
zonkType MetaT{} = unreachable "Plato.KindInfer.zonkType"

getMetaKvs :: MonadIO m => Kind -> Ki m (S.Set MetaKv)
getMetaKvs kn = do
        kn' <- zonkKind kn
        return (metaKvs kn')

metaKvs :: Kind -> S.Set MetaKv
metaKvs StarK = S.empty
metaKvs (ArrK kn1 kn2) = metaKvs kn1 `S.union` metaKvs kn2
metaKvs (MetaK kv) = S.singleton kv
