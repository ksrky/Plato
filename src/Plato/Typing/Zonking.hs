{-# LANGUAGE TupleSections #-}

module Plato.Typing.Zonking where

import Control.Monad.IO.Class
import Control.Monad.Reader

import Plato.Syntax.Typing
import Plato.Typing.Monad

zonkExpr :: MonadIO m => Expr -> m Expr
zonkExpr (VarE n) = return (VarE n)
zonkExpr (AppE fun arg) = AppE <$> zonkExpr `traverse` fun <*> zonkExpr `traverse` arg
zonkExpr (AbsE var mbty body) = AbsE var <$> zonkType `traverse` mbty <*> zonkExpr `traverse` body
zonkExpr (LetE bnds sigs body) = undefined

zonkType :: MonadIO m => Type -> m Type
zonkType (VarT tv) = return (VarT tv)
zonkType (ConT tc) = return (ConT tc)
zonkType (ArrT arg res) = ArrT <$> zonkType `traverse` arg <*> zonkType `traverse` res
zonkType (AllT tvs ty) = do
        tvs' <- forM tvs $ \(tv, mkn) -> (tv,) <$> zonkKind `traverse` mkn
        AllT tvs' <$> zonkType `traverse` ty
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
zonkBind (ValBind id mty exp) = ValBind id <$> zonkType `traverse` mty <*> zonkExpr `traverse` exp
zonkBind (TypeBind id mkn ty) = TypeBind id <$> zonkKind `traverse` mkn <*> zonkType `traverse` ty

zonkSpec :: MonadIO m => Spec -> m Spec
zonkSpec (ValSpec id ty) = ValSpec id <$> zonkType `traverse` ty
zonkSpec (TypeSpec id kn) = TypeSpec id <$> zonkKind kn

zonkDecl :: MonadIO m => Decl -> m Decl
zonkDecl (BindDecl bnd) = BindDecl <$> zonkBind bnd
zonkDecl (SpecDecl spc) = SpecDecl <$> zonkSpec spc
