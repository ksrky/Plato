module Plato.Core.Check where

{-
import Control.Exception.Safe
import Control.Monad
import Plato.Common.Error
import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Syntax.Core

----------------------------------------------------------------
-- Type equality check
----------------------------------------------------------------
istyabb :: Context -> Int -> Bool
istyabb ctx i = case getBinding ctx i of
        TyAbbBind tyT _ -> True
        _ -> False

gettyabb :: Context -> Int -> Ty
gettyabb ctx i = case getBinding ctx i of
        TyAbbBind tyT _ -> unLoc tyT
        _ -> error ""

computety :: Context -> Ty -> Maybe Ty
computety ctx tyT = case tyT of
        TyApp (TyAbs _ _ tyT12) tyT2 -> Just $ typeSubstTop tyT2 tyT12
        TyVar i _ | istyabb ctx i -> Just $ gettyabb ctx i
        _ -> Nothing

simplifyty :: Context -> Ty -> Ty
simplifyty ctx tyT =
        let tyT' = case simplifytyid ctx tyT of
                TyApp tyT1 tyT2 -> TyApp (simplifyty ctx tyT1) tyT2
                _ -> tyT
         in case computety ctx tyT' of
                Just tyT'' -> simplifyty ctx tyT''
                Nothing -> tyT'

simplifytyid :: Context -> Ty -> Ty
simplifytyid ctx tyT = case tyT of
        TyApp tyT1 tyT2 -> TyApp (simplifytyid ctx tyT1) tyT2
        _ -> tyT

tyeqv :: MonadThrow m => Context -> Ty -> Ty -> m ()
tyeqv ctx = tyeqv'
    where
        tyeqv' :: MonadThrow m => Ty -> Ty -> m ()
        tyeqv' tyS tyT = do
                let tyS' = simplifyty ctx tyS
                    tyT' = simplifyty ctx tyT
                case (tyS', tyT') of
                        (TyVar i _, _) | istyabb ctx i -> tyeqv' (gettyabb ctx i) tyT'
                        (_, TyVar i _) | istyabb ctx i -> tyeqv' tyS' (gettyabb ctx i)
                        (TyVar i _, TyVar j _) | i == j -> return ()
                        (TyArr tyS1 tyS2, TyArr tyT1 tyT2) -> do
                                tyeqv' tyS1 tyT1
                                tyeqv' tyS2 tyT2
                        (TyAll tyX1 _ tyS2, TyAll _ _ tyT2) -> do
                                ctx' <- addName tyX1 ctx
                                tyeqv ctx' tyS2 tyT2
                        (TyAbs tyX1 knKS1 tyS2, TyAbs _ knKT1 tyT2) | knKS1 == knKT1 -> do
                                ctx' <- addName tyX1 ctx
                                tyeqv ctx' tyS2 tyT2
                        {-(TyApp  (TyId _ b1) tyS2, _) -> do
                                i <- getVarIndex fi ctx b1
                                tyeqv' (TyApp  (gettyabb ctx i) tyS2) tyT'
                        (_, TyApp  (TyId _ b2) tyT2) -> do
                                i <- getVarIndex fi ctx b2
                                tyeqv' tyS' (TyApp  (gettyabb ctx i) tyT2)-}
                        (TyApp tyS1 tyS2, TyApp tyT1 tyT2) -> do
                                tyeqv' tyS1 tyT1
                                tyeqv' tyS2 tyT2
                        (TyRec x1 _ tyS2, TyRec _ _ tyT2) -> do
                                ctx' <- addName x1 ctx
                                tyeqv ctx tyS2 tyT2
                        (TyRecord fields1, TyRecord fields2) | length fields1 == length fields2 -> do
                                forM_ fields1 $ \(li1, tyTi1) -> case lookup li1 fields2 of
                                        Just tyTi2 -> tyeqv ctx tyTi1 tyTi2
                                        Nothing -> throwString $ "label " ++ show li1 ++ " not found"
                                forM_ fields2 $ \(li2, tyTi2) -> case lookup li2 fields1 of
                                        Just tyTi1 -> tyeqv ctx tyTi1 tyTi2
                                        Nothing -> throwString $ "label " ++ show li2 ++ " not found"
                        (TyRecord _, TyRecord _) -> throwString "Record field lengths are not equal"
                        (TyVariant fields1, TyVariant fields2) | length fields1 == length fields2 -> do
                                forM_ fields1 $ \(li1, tyTi1) -> case lookup li1 fields2 of
                                        Just tyTi2 -> zipWithM (tyeqv ctx) tyTi1 tyTi2
                                        Nothing -> throwString $ "label " ++ show li1 ++ " not found." ++ show fields1 ++ show fields2
                                forM_ fields2 $ \(li2, tyTi2) -> case lookup li2 fields1 of
                                        Just tyTi1 -> zipWithM (tyeqv ctx) tyTi1 tyTi2
                                        Nothing -> throwString $ "label " ++ show li2 ++ " not found"
                        (TyVariant fields1, TyVariant fields2) -> throwString "Variant field lengths are not equal"
                        _ -> throwString $ "type mismatch: " ++ pretty (ctx, tyS') ++ ", " ++ pretty (ctx, tyT')

----------------------------------------------------------------
-- Type check
----------------------------------------------------------------
getkind :: MonadThrow m => Context -> Int -> m Kind
getkind ctx i = case getBinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind tyT (Just knK) -> do
                knK' <- kindof ctx tyT
                unless (knK /= knK') $ throwString "Kind attachment failed"
                return knK
        TyAbbBind tyT Nothing -> kindof ctx tyT
        _ -> throwString $ "getkind: Wrong kind of binding for variable " ++ pretty (index2name ctx i)

kindof :: MonadThrow m => Context -> Ty -> m Kind
kindof ctx tyT = case tyT of
        TyArr tyT1 tyT2 -> do
                checkKindStar (getInfo tyT1) ctx tyT1
                checkKindStar (getInfo tyT2) ctx tyT2
                return KnStar
        TyVar i _ -> getkind ctx i
        TyAbs tyX knK1 tyT2 -> do
                ctx' <- addBinding tyX (TyVarBind knK1) ctx
                knK2 <- kindof ctx' tyT2
                return $ KnArr knK1 knK2
        TyApp tyT1 tyT2 -> do
                knK1 <- kindof ctx tyT1
                knK2 <- kindof ctx tyT2
                case knK1 of
                        KnArr knK11 knK12 | knK2 == knK11 -> return knK12
                        KnArr{} -> throwString (getInfo tyT1) "parameter kind mismatch"
                        _ -> throwString "arrow kind expected"
        TyAll tyX knK1 tyT2 -> do
                ctx' <- addBinding tyX (TyVarBind knK1) ctx
                checkKindStar (getInfo tyT2) ctx' tyT2
                return KnStar
        TyRec tyX knK1 tyT2 -> do
                ctx' <- addBinding tyX (TyVarBind knK1) ctx
                knK2 <- kindof ctx' tyT2
                unless (knK1 == knK2) $ throwString (getInfo tyT2) $ "Kind " ++ show knK1 ++ " expected"
                return KnStar
        TyRecord fieldtys -> do
                forM_ fieldtys $ \(l, tyS) -> checkKindStar fi ctx tyS
                return KnStar
        TyVariant fieldtys -> do
                forM_ fieldtys $ \(l, tys) -> do
                        forM_ tys $ \tyS -> checkKindStar fi ctx tyS
                return KnStar

checkKindStar :: MonadThrow m => Context -> Ty -> m ()
checkKindStar ctx tyT = do
        k <- kindof ctx tyT
        if k == KnStar
                then return ()
                else throwString $ "Kind * expected: " ++ pretty (ctx, tyT)

typeof :: (MonadThrow m, MonadFail m) => Context -> Term -> m Ty
typeof ctx t = case t of
        TmVar i _ -> getTypeFromContext ctx i
        TmAbs x tyT1 t2 -> do
                --checkKindStar  ctx tyT1
                ctx' <- addBinding x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyArr dummyInfo tyT1 (typeShift (-1) tyT2)
        TmApp t1 t2 -> do
                tyT1 <- typeof ctx t1
                tyT2 <- typeof ctx t2
                case tyT1 of
                        TyArr tyT11 tyT12 -> do
                                tyeqv ctx tyT2 tyT11
                                return tyT12
                        _ -> throwString (getInfo tyT1) "arrow type expected"
        TmTAbs tyX t2 -> do
                ctx' <- addBinding tyX (TyVarBind knK1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyAll dummyInfo tyX knK1 tyT2
        TmTApp t1 tyT2 -> do
                knKT2 <- kindof ctx tyT2
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyAll _ knK11 tyT12 | knK11 == knKT2 -> return $ typeSubstTop tyT2 tyT12
                        TyAll _ knK11 tyT12 -> throwString "Type argument has wrong kind"
                        _ -> throwString (getInfo tyT1) "universal type expected"
        TmLet x t1 t2 -> do
                tyT1 <- typeof ctx t1
                ctx' <- addBinding x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ typeShift (-1) tyT2
        TmFix t1 -> do
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyArr tyT11 tyT12 -> do
                                tyeqv ctx tyT12 tyT11
                                return tyT12
                        _ -> throwString "arrow type expected"
        TmFold tyS -> case simplifyty ctx tyS of
                TyRec _ _ tyT -> return $ TyArr (typeSubstTop tyS tyT) tyS
                _ -> throwString "recursive type expected"
        TmUnfold tyS -> case simplifyty ctx tyS of
                TyRec _ _ tyT -> return $ TyArr tyS (typeSubstTop tyS tyT)
                _ -> throwString "recursive type expected"
        TmProj t1 l -> do
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyRecord fieldtys -> case lookup l fieldtys of
                                Just tyT -> return tyT
                                Nothing -> throwString $ "label " ++ show l ++ " not found in " ++ pretty (ctx, tyT1)
                        _ -> throwString "Expected record type"
        TmRecord fields -> do
                fieldtys <- forM fields $ \(li, ti) -> do
                        tyTi <- typeof ctx ti
                        return (li, tyTi)
                return $ TyRecord fieldtys
        TmTag li ts1 tyT2 -> case simplifyty ctx tyT2 of
                TyVariant fieldtys -> case lookup li fieldtys of
                        Just tyTiExpected -> do
                                tyTi <- mapM (typeof ctx) ts1
                                mapM_ (uncurry $ tyeqv ctx) (zip tyTi tyTiExpected)
                                return tyT2
                        Nothing -> throwString $ "label " ++ pretty li ++ " not found"
                tyT2' -> throwString $ "Expected variant type, but got " ++ pretty (ctx, tyT2')
        TmCase t alts -> do
                tyT <- typeof ctx t
                case simplifytyid ctx $ simplifyty ctx tyT of
                        TyVariant fieldtys -> do
                                when (null fieldtys) $ return ()
                                (tyT1 : restTy) <- forM alts $ \(li, (ki, ti)) -> case lookup li fieldtys of
                                        Just tys -> do
                                                ctx' <- (`execStateT` ctx) $
                                                        forM_ tys $ \tyT -> StateT $ \ctx -> do
                                                                let ctx' = addFreshName dummyVarName (VarBind tyT) ctx
                                                                return ((), ctx')
                                                tyTi <- typeof ctx' ti
                                                return $ typeShift (- ki) tyTi
                                        Nothing | nullName li -> do
                                                let ctx' = addFreshName dummyVarName (VarBind $ TyVariant fieldtys) ctx
                                                tyTi <- typeof ctx' ti
                                                return $ typeShift (- ki) tyTi
                                        Nothing -> throwString $ "label " ++ show li ++ " not found"
                                forM_ restTy $ \tyTi -> tyeqv ctx tyTi tyT1
                                return tyT1
                        tyT' -> throwString $ "Expected variant type, but got " ++ pretty (ctx, tyT') ++ show (simplifytyid ctx tyT')

----------------------------------------------------------------
-- Type check of binding
----------------------------------------------------------------
checkBinding :: (MonadThrow m, MonadFail m) => Context -> Binding -> m ()
checkBinding ctx (TyAbbBind tyT (Just knK)) = do
        knK' <- kindof ctx tyT
        unless (knK == knK') $ throwString "Kind of binding does not match declared kind"
checkBinding ctx (TmAbbBind t (Just tyT)) = do
        tyT' <- typeof ctx t
        tyeqv ctx tyT tyT'
checkBinding _ _ = return ()

-}