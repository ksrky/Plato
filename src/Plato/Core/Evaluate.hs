module Plato.Core.Evaluate where

import Plato.Common.Error
import Plato.Common.Info
import qualified Plato.Common.Name as N
import Plato.Core.Context
import Plato.Core.Syntax

import Control.Exception.Safe
import Control.Monad.State

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isval :: Term -> Bool
isval t = case t of
        TmString{} -> True
        TmFloat{} -> True
        TmAbs{} -> True
        TmTag _ _ ts1 _ -> all isval ts1
        TmTAbs{} -> True
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 t)
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmApp _ (TmAbs _ (x, _) t12) v2 | isval v2 -> do
                        return $ termSubstTop v2 t12
                TmApp fi v1 t2 | isval v1 -> do
                        t2' <- eval1 t2
                        return $ TmApp fi v1 t2'
                TmApp fi t1 t2 -> do
                        t1' <- eval1 t1
                        return $ TmApp fi t1' t2
                TmTApp _ (TmTAbs _ (x, _) t11) tyT2 -> return $ tytermSubstTop tyT2 t11
                TmTApp fi t1 tyT2 -> do
                        t1' <- eval1 t1
                        return $ TmTApp fi t1' tyT2
                TmVar _ i n -> case getbinding ctx i of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmLet _ (x, v1) t2 | isval v1 -> Just $ termSubstTop v1 t2
                TmLet fi (x, t1) t2 -> do
                        t1' <- eval1 t1
                        Just $ TmLet fi (x, t1') t2
                TmTag _ l vs tyT | all isval vs -> Nothing
                TmTag fi l ts tyT -> do
                        ts' <- mapM eval1 ts
                        Just $ TmTag fi l ts' tyT
                TmCase _ (TmTag _ li vs11 _) alts | all isval vs11 -> case lookup li alts of
                        Just (_, body) -> do
                                return $ foldr termSubstTop body vs11
                        Nothing -> Nothing
                TmCase fi t1 alts -> do
                        t1' <- eval1 t1
                        Just $ TmCase fi t1' alts
                _ -> Nothing

----------------------------------------------------------------
-- kindof, typeof
----------------------------------------------------------------
getkind :: MonadThrow m => Context -> Int -> m Kind
getkind ctx i = case getbinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind _ (Just knK) -> return knK
        TyAbbBind _ Nothing -> throwString $ "No kind recorded for variable " ++ N.name2str (index2name ctx i)
        _ -> throwString $ "getkind: Wrong kind of binding for variable " ++ N.name2str (index2name ctx i)

kindof :: MonadThrow m => Ty -> Core m Kind
kindof tyT = case tyT of
        TyArr tyT1 tyT2 -> do
                knK1 <- kindof tyT1
                knK2 <- kindof tyT2
                unless (knK1 == KnStar && knK2 == KnStar) (throwString "star kind expected")
                return KnStar
        TyVar i _ -> do
                ctx <- get
                getkind ctx i
        TyAbs (tyX, knK1) tyT2 -> do
                addbinding tyX (TyVarBind knK1)
                knK2 <- kindof tyT2
                return $ KnArr knK1 knK2
        TyApp tyT1 tyT2 -> do
                knK1 <- kindof tyT1
                knK2 <- kindof tyT2
                case knK1 of
                        KnArr knK11 knK12 ->
                                if knK2 == knK11
                                        then return knK12
                                        else throwString "parameter kind mismatch"
                        _ -> throwString "arrow kind expected"
        TyAll (tyX, knK1) tyT2 -> do
                addbinding tyX (TyVarBind knK1)
                knK2 <- kindof tyT2
                when (knK2 /= KnStar) (throwString "Kind * expected")
                return KnStar
        _ -> return KnStar

typeof :: MonadThrow m => Term -> Core m Ty
typeof t = case t of
        TmString{} -> return TyString
        TmFloat{} -> return TyFloat
        TmVar fi i _ -> do
                ctx <- get
                return $ getTypeFromContext ctx i
        TmAbs fi (x, tyT1) t2 -> do
                addbinding x (VarBind tyT1)
                tyT2 <- typeof t2
                return $ TyArr tyT1 (typeShift (-1) tyT2)
        TmApp fi t1 t2 -> do
                tyT1 <- typeof t1
                tyT2 <- typeof t2
                case tyT1 of
                        TyArr tyT11 tyT12 | tyT2 == tyT11 -> return tyT12
                        TyArr tyT11 tyT12 -> throwError fi "parameter type mismatch"
                        _ -> throwError fi "arrow type expected"
        TmLet fi (x, t1) t2 -> do
                tyT1 <- typeof t1
                addbinding x (VarBind tyT1)
                tyT2 <- typeof t2
                return $ typeShift (-1) tyT2
        TmCase{} -> undefined
        TmTAbs fi (tyX, knK1) t2 -> do
                addbinding tyX (TyVarBind knK1)
                tyT2 <- typeof t2
                return $ TyAll (tyX, knK1) tyT2
        TmTApp fi t1 tyT2 -> do
                knKT2 <- kindof tyT2
                tyT1 <- typeof t1
                ctx <- get
                case tyT1 of
                        TyAll (_, knK11) tyT12 ->
                                if knK11 == knKT2
                                        then return $ typeSubstTop tyT2 tyT12
                                        else throwError fi "Type argument has wrong kind"
                        _ -> throwError fi "universal type expected"
        _ -> error ""

----------------------------------------------------------------
-- tyeqv
----------------------------------------------------------------
istyabb :: Context -> Int -> Bool
istyabb ctx i = case getbinding ctx i of
        TyAbbBind{} -> True
        _ -> False

gettyabb :: Context -> Int -> Maybe Ty
gettyabb ctx i = case getbinding ctx i of
        TyAbbBind tyT _ -> Just tyT
        _ -> Nothing

computety :: Context -> Ty -> Maybe Ty
computety ctx tyT = case tyT of
        TyVar i _ -> gettyabb ctx i
        _ -> Nothing

simplifyty :: Context -> Ty -> Ty
simplifyty ctx tyT = case computety ctx tyT of
        Just tyT' -> simplifyty ctx tyT'
        Nothing -> tyT

tyeqv :: Context -> Ty -> Ty -> Bool
tyeqv ctx tyS tyT = do
        let tyS = simplifyty ctx tyS
        let tyT = simplifyty ctx tyT
        case (tyS, tyT) of
                (TyArr tyS1 tyS2, TyArr tyT1 tyT2) -> tyeqv ctx tyS1 tyT1 && tyeqv ctx tyS2 tyT2
                (TyVar i _, _) | istyabb ctx i -> case gettyabb ctx i of
                        Just ty -> tyeqv ctx ty tyT
                        Nothing -> False
                (_, TyVar i _) | istyabb ctx i -> case gettyabb ctx i of
                        Just ty -> tyeqv ctx tyS ty
                        Nothing -> False
                (TyVar i _, TyVar j _) -> i == j
                (TyAll (tyX1, knK1) tyS2, TyAll (_, knK2) tyT2) -> do
                        let ctx' = addbinding_ tyX1 NameBind ctx
                        knK1 == knK2 && tyeqv ctx' tyS2 tyT2
                (TyString, TyString) -> True
                (TyFloat, TyFloat) -> True
                (TyVariant fields1, TyVariant fields2) -> (length fields1 == length fields2) && fseqv fields1 fields2
                    where
                        fseqv [] [] = True
                        fseqv ((li1, tyTi1) : f1) ((li2, tyTi2) : f2) = (li1 == li2) && tyeqvs ctx tyTi1 tyTi2 && fseqv f1 f2
                        fseqv _ _ = False
                        tyeqvs ctx [] [] = True
                        tyeqvs ctx (tyS : tySs) (tyT : tyTs) = tyeqv ctx tyS tyT && tyeqvs ctx tySs tyTs
                        tyeqvs ctx _ _ = False
                _ -> False
