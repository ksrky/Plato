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

zonkExpr :: MonadIO m => Expr -> Typ m Expr
zonkExpr (VarE n) = return (VarE n)
zonkExpr (AppE fun arg) = AppE <$> zonkExpr `traverse` fun <*> zonkExpr `traverse` arg
zonkExpr (AbsE var mbty body) = AbsE var <$> zonkType `traverse` mbty <*> zonkExpr `traverse` body
zonkExpr (TAppE body ty_args) = TAppE body <$> mapM zonkType ty_args
zonkExpr (TAbsE ty_vars body) = TAbsE ty_vars <$> zonkExpr `traverse` body
zonkExpr (LetE decs body) = do
        decs' <-
                mapM
                        ( \case
                                BindDecl (ValueBind id mty exp) -> BindDecl <$> (ValueBind id <$> zonkType `traverse` mty <*> zonkExpr `traverse` exp)
                                BindDecl (TypeBind id mkn ty) -> BindDecl <$> (TypeBind id <$> zonkKind `traverse` mkn <*> zonkType `traverse` ty)
                                BindDecl (ModuleBind id mod) -> BindDecl <$> (ModuleBind id <$> zonkModule mod)
                                SpecDecl (ValueSpec id ty) -> SpecDecl <$> (ValueSpec id <$> zonkType ty)
                                SpecDecl (TypeSpec id kn) -> SpecDecl <$> (TypeSpec id <$> zonkKind kn)
                                dec -> return dec
                        )
                        decs
        LetE decs' <$> zonkExpr `traverse` body
zonkExpr (CaseE e mbty alts) =
        CaseE
                <$> zonkExpr `traverse` e
                <*> zonkType `traverse` mbty
                <*> forM alts (\(pat, body) -> (pat,) <$> zonkExpr `traverse` body)
zonkExpr _ = unreachable "TypeCheck.Utils.zonkExpr"

zonkType :: MonadIO m => Type -> Typ m Type
zonkType (VarT tv) = return (VarT tv)
zonkType (ConT tc) = return (ConT tc)
zonkType (ArrT arg res) = ArrT <$> zonkType `traverse` arg <*> zonkType `traverse` res
zonkType (AllT tvs ty) = do
        tvs' <- forM tvs $ \(tv, mkn) -> (tv,) <$> zonkKind `traverse` mkn
        AllT tvs' <$> zonkType `traverse` ty
zonkType (AppT fun arg) = AppT <$> zonkType `traverse` fun <*> zonkType `traverse` arg
zonkType (AbsT x mkn ty) = AbsT x <$> zonkKind `traverse` mkn <*> zonkType `traverse` ty
zonkType (RecordT fields) = do
        fields' <- forM fields $ \(x, ty) -> (x,) <$> zonkType `traverse` ty
        return $ RecordT fields'
zonkType (SumT fields) = do
        fields' <- forM fields $ \(x, tys) -> (x,) <$> mapM (zonkType `traverse`) tys
        return $ SumT fields'
zonkType (MetaT tv) = do
        mb_ty <- readMetaTv tv
        case mb_ty of
                Nothing -> return (MetaT tv)
                Just ty -> do
                        ty' <- zonkType ty
                        writeMetaTv tv ty'
                        return ty'

zonkKind :: MonadIO m => Kind -> Typ m Kind
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

zonkModule :: MonadIO m => Module -> Typ m Module
zonkModule = undefined