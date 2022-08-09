module Plato.Core.Context where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.Vect
import Plato.Core.Syntax

import Control.Exception.Safe
import Control.Monad.State
import Plato.Common.Info

----------------------------------------------------------------
-- Core monad
----------------------------------------------------------------
type Core m = StateT Context m

evalCore :: Monad m => Core m a -> Core m a
evalCore f = do
        ctx <- get
        val <- f
        put ctx
        return val

----------------------------------------------------------------
-- Context
----------------------------------------------------------------
type Context = Vect (Name, Binding)

emptyContext :: Context
emptyContext = empty

initContext :: Context --tmp
initContext = cons (str2tyConName "String", TyVarBind KnStar) $ cons (str2tyConName "Float", TyVarBind KnStar) emptyContext

addbinding :: Name -> Binding -> Context -> Context
addbinding x bind = cons (x, bind)

addname :: Name -> Context -> Context
addname x = cons (x, NameBind)

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
        TmAbbBind t tyT_opt -> TmAbbBind (termShift d t) (typeShift d <$> tyT_opt)
        TyAbbBind tyT opt -> TyAbbBind (typeShift d tyT) opt

getbinding :: Context -> Int -> Binding
getbinding ctx i = bindingShift (i + 1) (snd $ ctx ! i)

getTypeFromContext :: MonadThrow m => Info -> Context -> Int -> m Ty
getTypeFromContext fi ctx i = case getbinding ctx i of
        VarBind tyT -> return tyT
        TmAbbBind _ (Just tyT) -> return tyT
        TmAbbBind _ Nothing -> throwError fi $ "No type recorded for variable " ++ show (index2name ctx i)
        _ -> throwError fi $ "Wrong kind of binding for variable " ++ show (index2name ctx i)

getVarIndex :: MonadFail m => Name -> Context -> m Int
getVarIndex x ctx = case elemIndex x (vmap fst ctx) of
        Just i -> return i
        Nothing -> fail $ "Unbound variable name: '" ++ show x ++ "'"