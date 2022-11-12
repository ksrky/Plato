{-# LANGUAGE OverloadedStrings #-}

module Plato.Core.Context where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.Name.Global
import Plato.Common.SrcLoc
import Plato.Core.Subst
import Plato.Syntax.Core

import Control.Exception.Safe
import Control.Monad
import qualified Data.Text as T
import qualified Data.Vector as V
import Prettyprinter

type Context = V.Vector (Name, Binding)

emptyContext :: Context
emptyContext = V.empty

lookupContext :: Name -> Context -> Maybe Binding
lookupContext k ctx = do
        ((x, y), tl) <- V.uncons ctx
        if k == x then Just y else lookupContext k tl

addBinding :: MonadThrow m => Located Name -> Binding -> Context -> m Context
addBinding x bind ctx =
        {-case lookupContext x ctx of
        Just _ | g_sort x /= System -> throwLocErr (g_loc x) $ "Conflicting definition of" <+> pretty x
        _ -> -} return $ V.cons (unLoc x, bind) ctx

addName :: MonadThrow m => Located Name -> Context -> m Context
addName x = addBinding x NameBind

addNameList :: MonadThrow m => [Located Name] -> Context -> m Context
addNameList = flip $ foldM (flip addName)

addFreshName :: GlbName -> Context -> Context
addFreshName x ctx = case lookupContext x ctx of
        Just _ -> addFreshName (newName (g_name x){nameText = T.snoc (nameText (g_name x)) '\''}) ctx
        Nothing -> V.cons (x, NameBind) ctx

pickFreshName :: GlbName -> Context -> (GlbName, Context)
pickFreshName x ctx = case lookupContext x ctx of
        Just _ -> pickFreshName (newName (g_name x){nameText = T.snoc (nameText (g_name x)) '\''}) ctx
        Nothing -> (x, V.cons (x, NameBind) ctx)

index2name :: Context -> Int -> GlbName
index2name ctx x = fst (ctx V.! x)

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        VarBind tyT -> VarBind (typeShift d <$> tyT)
        TyVarBind knK -> TyVarBind knK
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d <$> t) (typeShift d <$> tyT_opt)
        TyAbbBind tyT opt -> TyAbbBind (typeShift d <$> tyT) opt

getBinding :: Context -> Int -> Binding
getBinding ctx i = bindingShift (i + 1) (snd $ ctx V.! i)

getType :: MonadThrow m => Span -> Context -> Int -> m Ty
getType sp ctx i = case getBinding ctx i of
        VarBind tyT -> return $ unLoc tyT
        TmAbbBind _ tyT -> return $ unLoc tyT
        _ -> throwLocErr sp $ "Wrong kind of binding for variable" <+> pretty (index2name ctx i)

getKind :: MonadThrow m => Context -> Int -> m Kind
getKind ctx i = case getBinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind _ knK -> return knK
        _ -> throwError $ hsep ["getkind: Wrong kind of binding for variable", pretty (index2name ctx i)]

getVarIndex :: MonadThrow m => Context -> GlbName -> m Int
getVarIndex ctx x = case V.elemIndex x (V.map fst ctx) of
        Just i -> return i
        Nothing -> throwLocErr (g_loc x) $ "Unbound variable name: '" <> pretty x <> "'"