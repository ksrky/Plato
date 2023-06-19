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

zonkExpr :: MonadIO m => Expr 'Typed -> m (Expr 'Typed)
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
zonkExpr (CaseEok match ann alts) = do
        match' <- zonkExpr `traverse` match
        ann' <- zonkType ann
        alts' <- mapM (\(pats, exp) -> (pats,) <$> zonkExpr `traverse` exp) alts
        return $ CaseEok match' ann' alts'

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

zonkDefn :: MonadIO m => Defn 'Typed -> m (Defn 'Typed)
-- zonkDefn (ValDefn id exp) = ValDefn id <$> zonkExpr `traverse` exp
zonkDefn (FunDefnok id exp) =
        FunDefnok id <$> zonkExpr `traverse` exp
zonkDefn (TypDefn id ty) = TypDefn id <$> zonkType `traverse` ty
zonkDefn (DatDefnok id sig params constrs) =
        DatDefnok id
                <$> zonkKind sig
                <*> mapM (\(p, kn) -> (p,) <$> zonkKind kn) params
                <*> mapM (\(con, ty) -> (con,) <$> zonkType `traverse` ty) constrs

zonkSpec :: MonadIO m => Spec -> m Spec
zonkSpec (ValSpec id ty) = ValSpec id <$> zonkType `traverse` ty
zonkSpec (TypSpec id kn) = TypSpec id <$> zonkKind kn

zonkDecl :: MonadIO m => Decl 'Typed -> m (Decl 'Typed)
zonkDecl (DefnDecl def) = DefnDecl <$> zonkDefn def
zonkDecl (SpecDecl spc) = SpecDecl <$> zonkSpec spc
