module Plato.Typing.KindInfer where

{-}
appKind :: Kind -> Kind -> StateT TypState TypThrow Kind
appKind k1 k2 = case k1 of
        ArrKind k11 k12 | k11 == k2 -> return k12
        ArrKind (VarKind x) k12 -> updateKind x k2 >> return k12
        VarKind x -> do
                n <- freshKindName $ unLoc x
                updateKind x (ArrKind k2 (VarKind n))
                return $ VarKind n
        _ -> return StarKind -- error

infer :: Located Type -> StateT TypState TypThrow Kind
infer ty = case unLoc ty of
        VarType x -> getKind $ unLoc x
        ArrType ty1 ty2 -> do
                infer ty1
                infer ty2
                return StarKind
        AllType x _ ty -> do
                freshKindName $ unLoc x
                infer ty
        AbsType x _ ty -> do
                freshKindName $ unLoc x
                k2 <- infer ty
                k1 <- getKind $ unLoc x
                return $ ArrKind k1 k2
        AppType ty1 ty2 -> do
                k2 <- infer ty2
                k1 <- infer ty1
                appKind k1 k2
        RecType x ty -> do
                freshKindName $ unLoc x
                infer ty
        RecordType fields -> do
                mapM_ (\(_, ty) -> infer ty) fields
                return StarKind
        SumType fields -> do
                forM_ fields $ \(_, tys) -> mapM infer tys
                return StarKind
-}