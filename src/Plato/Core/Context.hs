module Plato.Core.Context where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.Vect
import Plato.Core.Syntax

import Control.Exception.Safe
import Control.Monad.State
import Plato.Common.Info

type Context = Vect (Name, Binding)

emptyContext :: Context
emptyContext = empty

addbinding :: MonadThrow m => Info -> Name -> Binding -> Context -> m Context
addbinding fi x bind ctx = case look x ctx of
        Just _ -> throwError fi $ "Conflicting definition of " ++ show x
        _ -> return $ cons (x, bind) ctx

addname :: MonadThrow m => Info -> Name -> Context -> m Context
addname fi x = addbinding fi x NameBind

pickfreshname :: Name -> Context -> (Name, Context)
pickfreshname x ctx = case look x ctx of
        Just _ -> pickfreshname (appendstr x "'") ctx
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

getbinding :: Context -> Int -> Binding
getbinding ctx i = if i + 1 < 0 then error $ show i else bindingShift (i + 1) (snd $ ctx ! i)

getTypeFromContext :: MonadThrow m => Info -> Context -> Int -> m Ty
getTypeFromContext fi ctx i = case getbinding ctx i of
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
