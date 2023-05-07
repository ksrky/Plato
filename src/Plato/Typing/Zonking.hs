{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Plato.Typing.Zonking where

import Control.Monad.IO.Class
import Control.Monad.Reader

import Plato.Common.Error
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Module
import Plato.Syntax.Typing.Type
import Plato.Typing.Monad

zonkExpr :: MonadIO m => Expr -> m Expr
zonkExpr (VarE n) = return (VarE n)
zonkExpr (AppE fun arg) = AppE <$> zonkExpr `traverse` fun <*> zonkExpr `traverse` arg
zonkExpr (AbsE var mbty body) = AbsE var <$> zonkType `traverse` mbty <*> zonkExpr `traverse` body
zonkExpr (TAppE body ty_args) = TAppE body <$> mapM zonkType ty_args
zonkExpr (TAbsE ty_vars body) = TAbsE ty_vars <$> zonkExpr `traverse` body
zonkExpr (LetE decs body) = do
        decs' <- forM decs $ \case
                BindDecl (ValueBind id mty exp) -> BindDecl <$> (ValueBind id <$> zonkType `traverse` mty <*> zonkExpr `traverse` exp)
                -- BindDecl (TypeBind id mkn ty) -> BindDecl <$> (TypeBind id <$> zonkKind `traverse` mkn <*> zonkType `traverse` ty)
                BindDecl (DataBind id fields) -> BindDecl <$> (DataBind id <$> mapM (\(con, ty) -> (con,) <$> zonkType `traverse` ty) fields)
                BindDecl (ModuleBind id mod) -> BindDecl <$> (ModuleBind id <$> zonkModule `traverse` mod)
                SpecDecl (ValueSpec id ty) -> SpecDecl <$> (ValueSpec id <$> zonkType `traverse` ty)
                SpecDecl (TypeSpec id kn) -> SpecDecl <$> (TypeSpec id <$> zonkKind kn)
                dec -> return dec

        LetE decs' <$> zonkExpr `traverse` body
zonkExpr (MatchE matches alts) =
        MatchE
                <$> mapM (\(e, mty) -> (,) <$> zonkExpr `traverse` e <*> zonkType `traverse` mty) matches
                <*> forM alts (\(pat, body) -> (pat,) <$> zonkExpr `traverse` body)
zonkExpr _ = unreachable "TypeCheck.Utils.zonkExpr"

zonkType :: MonadIO m => Type -> m Type
zonkType (VarT tv) = return (VarT tv)
zonkType (ConT tc) = return (ConT tc)
zonkType (ArrT arg res) = ArrT <$> zonkType `traverse` arg <*> zonkType `traverse` res
zonkType (AllT tvs ty) = do
        tvs' <- forM tvs $ \(tv, mkn) -> (tv,) <$> zonkKind `traverse` mkn
        AllT tvs' <$> zonkType `traverse` ty
zonkType (AppT fun arg) = AppT <$> zonkType `traverse` fun <*> zonkType `traverse` arg
zonkType (AbsT x mkn ty) = AbsT x <$> zonkKind `traverse` mkn <*> zonkType `traverse` ty
{-zonkType (RecordT fields) = do
        fields' <- forM fields $ \(x, ty) -> (x,) <$> zonkType `traverse` ty
        return $ RecordT fields'
zonkType (SumT fields) = do
        fields' <- forM fields $ \(x, tys) -> (x,) <$> mapM (zonkType `traverse`) tys
        return $ SumT fields'-}
zonkType (MetaT tv) = do
        mb_ty <- readMetaTv tv
        case mb_ty of
                Nothing -> return (MetaT tv)
                Just ty -> do
                        ty' <- zonkType ty
                        writeMetaTv tv ty'
                        return ty'

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

zonkBind :: MonadIO m => Bind -> m Bind
zonkBind (ValueBind id mty exp) = ValueBind id <$> zonkType `traverse` mty <*> zonkExpr `traverse` exp
-- zonkBind (TypeBind id mkn ty) = TypeBind id <$> zonkKind `traverse` mkn <*> zonkType `traverse` ty
zonkBind (DataBind id fields) = DataBind id <$> mapM (\(con, ty) -> (con,) <$> zonkType `traverse` ty) fields
zonkBind (ModuleBind id mod) = ModuleBind id <$> zonkModule `traverse` mod

zonkSpec :: MonadIO m => Spec -> m Spec
zonkSpec (ValueSpec id ty) = ValueSpec id <$> zonkType `traverse` ty
zonkSpec (TypeSpec id kn) = TypeSpec id <$> zonkKind kn

zonkDecl :: MonadIO m => Decl -> m Decl
zonkDecl (BindDecl bnd) = BindDecl <$> zonkBind bnd
zonkDecl (SpecDecl spc) = SpecDecl <$> zonkSpec spc
zonkDecl dec = return dec

zonkModule :: MonadIO m => Module -> m Module
zonkModule (Module decs) = Module <$> mapM zonkDecl decs