{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing.Zonking (
        zonkExpr,
        zonkType,
        zonkQuants,
        zonkKind,
        zonkDecl,
) where

import Control.Monad.IO.Class

import Plato.Syntax.Typing
import Plato.Typing.Monad

zonkExpr :: MonadIO m => Expr 'TcDone -> m (Expr 'TcDone)
zonkExpr (VarE n) = return (VarE n)
zonkExpr (AppE fun arg) = AppE <$> zonkExpr `traverse` fun <*> zonkExpr `traverse` arg
zonkExpr (AbsEok var ty body) = AbsEok var <$> zonkType ty <*> zonkExpr body
zonkExpr (TAppE exp tys) = TAppE <$> zonkExpr exp <*> mapM zonkType tys
zonkExpr (TAbsE qnts body) = do
        qnts' <- mapM (\(tv, kn) -> (tv,) <$> zonkKind kn) qnts
        TAbsE qnts' <$> zonkExpr body
zonkExpr (LetEok bnds spcs body) = do
        bnds' <- mapM (\(id, exp) -> (id,) <$> zonkExpr `traverse` exp) bnds
        spcs' <- mapM (\(id, ty) -> (id,) <$> zonkType `traverse` ty) spcs
        body' <- zonkExpr `traverse` body
        return $ LetEok bnds' spcs' body'
zonkExpr (CaseE match alts) = do
        match' <- zonkExpr `traverse` match
        alts' <- mapM (\(pats, exp) -> (pats,) <$> zonkExpr `traverse` exp) alts
        return $ CaseE match' alts'

zonkType :: MonadIO m => Type -> m Type
zonkType (VarT tv) = return (VarT tv)
zonkType (ConT tc) = return (ConT tc)
zonkType (ArrT arg res) = ArrT <$> zonkType `traverse` arg <*> zonkType `traverse` res
zonkType (AllT qnts ty) = AllT <$> zonkQuants qnts <*> zonkType `traverse` ty
zonkType (AppT fun arg) = do
        AppT <$> zonkType `traverse` fun <*> zonkType `traverse` arg
zonkType (MetaT tv) = do
        mb_ty <- readMetaTv tv
        case mb_ty of
                Nothing -> return (MetaT tv)
                Just ty -> do
                        ty' <- zonkType ty
                        writeMetaTv tv ty'
                        return ty'

zonkQuants :: MonadIO m => [Quant] -> m [Quant]
zonkQuants = mapM $ \(tv, kn) -> (tv,) <$> zonkKind kn

zonkKind :: MonadIO m => Kind -> m Kind
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

zonkBind :: MonadIO m => Bind 'TcDone -> m (Bind 'TcDone)
-- zonkBind (ValBind id exp) = ValBind id <$> zonkExpr `traverse` exp
zonkBind (FunBindok id exp) =
        FunBindok id <$> zonkExpr `traverse` exp
zonkBind (TypBind id ty) = TypBind id <$> zonkType `traverse` ty
zonkBind (DatBindok id sig params constrs) =
        DatBindok id
                <$> zonkKind sig
                <*> mapM (\(p, kn) -> (p,) <$> zonkKind kn) params
                <*> mapM (\(con, ty) -> (con,) <$> zonkType `traverse` ty) constrs

zonkSpec :: MonadIO m => Spec -> m Spec
zonkSpec (ValSpec id ty) = ValSpec id <$> zonkType `traverse` ty
zonkSpec (TypSpec id kn) = TypSpec id <$> zonkKind kn

zonkDecl :: MonadIO m => Decl 'TcDone -> m (Decl 'TcDone)
zonkDecl (BindDecl bnd) = BindDecl <$> zonkBind bnd
zonkDecl (SpecDecl spc) = SpecDecl <$> zonkSpec spc
