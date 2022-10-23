module Plato.Core.Context where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Common.Vect
import Plato.Syntax.Core

import Control.Exception.Safe
import Control.Monad
import qualified Data.Text as T
import qualified Data.Vector as V

type Context = V.Vector (Name, Binding)

emptyContext :: Context
emptyContext = empty

addBinding :: MonadThrow m => Located Name -> Binding -> Context -> m Context
addBinding (L sp x) bind ctx = case look x ctx of
        Just _ -> throwLocatedErr sp $ "Conflicting definition of " ++ show x
        _ -> return $ cons (x, bind) ctx

addName :: MonadThrow m => Located Name -> Context -> m Context
addName x = addBinding x NameBind

addNames :: MonadThrow m => [Located Name] -> Context -> m Context
addNames = flip $ foldM (flip addName)

addFreshName :: Name -> Binding -> Context -> Context
addFreshName x bind ctx = case look x ctx of
        Just _ -> addFreshName (x{nameText = T.snoc (nameText x) '\''}) bind ctx
        Nothing -> cons (x, bind) ctx

pickFreshName :: Name -> Context -> (Name, Context)
pickFreshName x ctx = case look x ctx of
        Just _ -> pickFreshName (x{nameText = T.snoc (nameText x) '\''}) ctx
        Nothing -> (x, cons (x, NameBind) ctx)

index2name :: Context -> Int -> Name
index2name ctx x = fst (ctx ! x)

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        VarBind tyT -> VarBind (typeShift d <$> tyT)
        TyVarBind knK -> TyVarBind knK
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d <$> t) ((typeShift d <$>) <$> tyT_opt)
        TyAbbBind tyT opt -> TyAbbBind (typeShift d <$> tyT) opt

getBinding :: Context -> Int -> Binding
getBinding ctx i = if i + 1 < 0 then error $ show i else bindingShift (i + 1) (snd $ ctx ! i)

getTypeFromContext :: MonadThrow m => Span -> Context -> Int -> m (Located Ty)
getTypeFromContext sp ctx i = case getBinding ctx i of
        VarBind tyT -> return tyT
        TmAbbBind _ (Just tyT) -> return tyT
        TmAbbBind _ Nothing -> throwLocatedErr sp $ "No type recorded for variable " ++ show (index2name ctx i)
        _ -> throwLocatedErr sp $ "Wrong kind of binding for variable " ++ show (index2name ctx i)

getVarIndex :: MonadThrow m => Context -> Located Name -> m Int
getVarIndex ctx (L sp x) = case elemIndex x (vmap fst ctx) of
        Just i -> return i
        Nothing -> throwLocatedErr sp $ "Unbound variable name: '" ++ show x ++ "'"

getVarIndex' :: Context -> Name -> Int
getVarIndex' ctx x = case elemIndex x (vmap fst ctx) of
        Just i -> i
        Nothing -> unreachable $ "Unbound variable name: '" ++ show x ++ "'"

isVarExist :: Context -> Name -> Bool
isVarExist ctx x = case elemIndex x (vmap fst ctx) of
        Just _ -> True
        Nothing -> False