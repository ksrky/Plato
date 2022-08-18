module Plato.Core.Context where

import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Name
import Plato.Common.Vect
import Plato.Core.Syntax

import Control.Exception.Safe
import Control.Monad.State

type Context = Vect (Name, Binding)

emptyContext :: Context
emptyContext = empty

addBinding :: MonadThrow m => Info -> Name -> Binding -> Context -> m Context
addBinding fi x bind ctx = case look x ctx of
        Just _ -> throwError fi $ "Conflicting definition of " ++ show x
        _ -> return $ cons (x, bind) ctx

addName :: MonadThrow m => Info -> Name -> Context -> m Context
addName fi x = addBinding fi x NameBind

addFreshName :: Name -> Binding -> Context -> Context
addFreshName x bind ctx = case look x ctx of
        Just _ -> addFreshName (appendstr x "'") bind ctx
        Nothing -> cons (x, bind) ctx

pickFreshName :: Name -> Context -> (Name, Context)
pickFreshName x ctx = case look x ctx of
        Just _ -> pickFreshName (appendstr x "'") ctx
        Nothing -> (x, cons (x, NameBind) ctx)

index2name :: Context -> Int -> Name
index2name ctx x = fst (ctx ! x)

bindingShift :: Int -> Binding -> Binding
bindingShift d bind = case bind of
        NameBind -> NameBind
        VarBind tyT -> VarBind (typeShift d tyT)
        TyVarBind knK -> TyVarBind knK
        TmAbbBind t tyT_opt -> if d < 0 then error $ show d else TmAbbBind (termShift d t) (typeShift d <$> tyT_opt)
        TyAbbBind tyT opt -> TyAbbBind (typeShift d tyT) opt

getBinding :: Context -> Int -> Binding
getBinding ctx i = if i + 1 < 0 then error $ show i else bindingShift (i + 1) (snd $ ctx ! i)

getTypeFromContext :: MonadThrow m => Info -> Context -> Int -> m Ty
getTypeFromContext fi ctx i = case getBinding ctx i of
        VarBind tyT -> return tyT
        TmAbbBind _ (Just tyT) -> return tyT
        TmAbbBind _ Nothing -> throwError fi $ "No type recorded for variable " ++ show (index2name ctx i)
        _ -> throwError fi $ "Wrong kind of binding for variable " ++ show (index2name ctx i)

getVarIndex :: MonadThrow m => Info -> Context -> Name -> m Int
getVarIndex fi ctx x = case elemIndex x (vmap fst ctx) of
        Just i -> return i
        Nothing -> throwError fi $ "Unbound variable name: '" ++ show x ++ "'"

isVarExist :: Context -> Name -> Bool
isVarExist ctx x = case elemIndex x (vmap fst ctx) of
        Just _ -> True
        Nothing -> False
